(ns net.danielcompton.defn-spec-alpha.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as specs]
            [clojure.walk :as walk]))

;; First, monkey-patch the specs.alpha spec so unforms and conform are inverses of each other.
;; https://blog.klipse.tech/clojure/2019/03/08/spec-custom-defn.html which in turn cites
;; https://github.com/Engelberg/better-cond
;; I'm not comfortable with directly monkey-patching because I don't want to have to worry
;; about evaluation order, I'd prefer to alter the spec fully and house it in this namespace.
;; The original specs are specified here:
;; https://github.com/clojure/core.specs.alpha/blob/master/src/main/clojure/clojure/core/specs/alpha.clj


(defn arg-list-unformer [a]
  (vec 
   (if (and (coll? (last a)) (= '& (first (last a))))
     (concat (drop-last a) (last a))
     a)))

(s/def ::param-list
       (s/and
        vector?
        (s/conformer identity arg-list-unformer)
        (s/cat :args (s/* ::specs/binding-form)
               :varargs (s/? (s/cat :amp #{'&} :form ::specs/binding-form)))))

(s/def ::params+body
  (s/cat :params ::param-list
         :body (s/alt :prepost+body (s/cat :prepost map?
                                           :body (s/+ any?))
                      :body (s/* any?))))

(s/def ::defn-args
  (s/cat :fn-name simple-symbol?
         :docstring (s/? string?)
         :meta (s/? map?)
         :fn-tail (s/alt :arity-1 ::params+body
                         :arity-n (s/cat :bodies (s/+ (s/spec ::params+body))
                                         :attr-map (s/? map?)))))

;; Annotated defn specs

(s/def ::spec-annotation (s/cat :spec-literal #{:-} :spec any?))

(defn annotated-arg-list-unformer [a]
  (let [args-form (->> a
                       (arg-list-unformer)
                       (map (fn [x]
                              (cond (not (seq? x)) (list x)
                                    (map? x) (list x)
                                    :else x))))]
    (vec (apply concat args-form))))

(defn annotated-defn-unformer [a]
  (if (and (coll? a) (= :- (first (first (rest a)))))
    (concat (take 1 a) (first (rest a)) (rest (rest a)))
    a))

(s/def ::local-name (s/and simple-symbol? #(not= '& %)))

(s/def ::annotated-map-binding-form (s/merge ::specs/map-bindings ::specs/map-special-binding))

(s/def ::annotated-binding-form
  (s/alt :local-symbol (s/cat :local-name ::local-name
                              :annotation (s/? (s/cat :spec-literal #{:-} :spec any?)))
         :seq-destructure (s/cat :seq-binding-form ::specs/seq-binding-form
                                 :annotation (s/? (s/cat :spec-literal #{:-} :spec any?)))
         :map-destructure (s/cat :map-binding-form ::specs/map-binding-form
                                 :annotation (s/? (s/cat :spec-literal #{:-} :spec any?)))))

(s/def ::annotated-param-list
  (s/and
   vector?
   (s/conformer identity annotated-arg-list-unformer)
   (s/cat :args (s/* ::annotated-binding-form)
          :varargs (s/? (s/cat :amp #{'&} :form ::annotated-binding-form)))))

(s/def ::annotated-params+body
  (s/cat :params ::annotated-param-list
         :body (s/alt :prepost+body (s/cat :prepost map?
                                           :body (s/+ any?))
                      :body (s/* any?))))

(s/def ::annotated-defn-args
  (s/and
   (s/conformer identity annotated-defn-unformer)
   (s/cat :fn-name simple-symbol?
          :ret-annotation (s/? ::spec-annotation)
          :docstring (s/? string?)
          :meta (s/? map?)
          :fn-tail (s/alt :arity-1 ::annotated-params+body
                          :arity-n (s/cat :bodies (s/+ (s/spec ::annotated-params+body))
                                          :attr-map (s/? map?))))))


;; Annotated form
'{:fn-name a,
 :ret-annotation {:spec-literal :-, :spec map?},
 :fn-tail
 [:arity-1
  {:params
   {:args
    [[:local-symbol
      {:local-name b, :annotation {:spec-literal :-, :spec int?}}]
     [:local-symbol
      {:local-name c, :annotation {:spec-literal :-, :spec int?}}]],
    :varargs
    {:amp &,
     :form
     [:local-symbol
      {:local-name so-on,
       :annotation {:spec-literal :-, :spec int?}}]}},
   :body [:body [(+ b c)]]}]}

'{:fn-name a,
 :fn-tail
 [:arity-1
  {:params
   {:args [[:local-symbol b] [:local-symbol c]],
    :varargs {:amp &, :form [:local-symbol so-on]}},
   :body [:body [(+ b c)]]}]}

;; Form without annotation


(defn annotated-args->args [ast]
  (walk/postwalk (fn [x]
                   (cond (and (coll? x) (= (first x) :local-symbol))
                         [:local-symbol (:local-name (last x))]
                         (and (coll? x) (= (first x) :map-destructure))
                         [:map-destructure (:map-binding-form (last x))]
                         (and (coll? x) (= (first x) :seq-destructure))
                         [:seq-destructure (:seq-binding-form (last x))]
                         :else x)) ast))

(defn annotated-varargs->args [ast]
  (walk/postwalk (fn [x]
                   (cond (and (coll? x) (:annotation x)) (dissoc x :annotation)
                         (and (map-entry? x) (= :seq-binding-form (key x))) (val x)
                         :else x)) ast))

(defn annotated-defn->defn [ast]
  (walk/prewalk (fn [x]
                  (cond (and (map-entry? x) (= :args (key x))) (annotated-args->args x)
                        (and (map-entry? x) (= :varargs (key x))) (annotated-varargs->args x)
                        :else x)) ast))

(defn nil->any? [spec]
  (if (nil? spec) any?
      spec))

(defn nils->any? [specs]
  (map nil->any? specs))

(defn gen-argument-keys [args]
  "Given a list of arguments obtained by conforming the ::annotated-defn-args spec,
   generate a list of keywords to label the spec in s/cat. Because map destructures are
   sometimes anonymous and seq destructures are always anonymous, we generate unique keys
   to annote them and aid in legibility when functions are instrumented."
  (let [keys-accumulator
        (reduce (fn [acc [type params]]
                  (case type
                    :local-symbol (update acc :arg-keys conj (keyword (:local-name params)))
                    :map-destructure (-> acc
                                         (update :arg-keys
                                                 conj
                                                 (keyword
                                                  (keyword (str "map-destructure-" (:map-destructure-count acc)))))
                                         (update :map-destructure-count inc))
                    :seq-destructure (-> acc
                                         (update :arg-keys
                                                 conj
                                                 (keyword (str "seq-destructure-" (:seq-destructure-count acc))))
                                         (update :seq-destructure-count inc))))
                {:map-destructure-count 1
                 :seq-destructure-count 1
                 :arg-keys []}
                args)]
    (:arg-keys keys-accumulator)))

(gen-argument-keys '[[:local-symbol
                      {:local-name b, :annotation {:spec-literal :-, :spec int?}}]
                     [:seq-destructure
                      {:seq-binding-form
                       {:forms [[:local-symbol c] [:local-symbol d]]}}]])

(defn arity-labels []
  (map (fn [x] (keyword (str "arity-" x))) (iterate inc 1)))

(defn combine-arg-specs [{:keys [fn-tail]}]
  ;; In the event of arity-1, check if anything is specified at all. If not, return nil
  ;; If so, run through each form, generating either the annotation with label or the
  ;; label with any?
  (case (first fn-tail)
    :arity-1
    (let [{:keys [args varargs]} (:params (last fn-tail))
          arg-specs (into [] (map #(get-in (last %) [:annotation :spec])) args)
          arg-names (gen-argument-keys args)
          vararg-spec (-> varargs
                          :form
                          last
                          :annotation
                          :spec)]
      ;; If no arguments are specced, return nil
      (when (some identity (conj arg-specs vararg-spec))
        (let [specced-args (vec (interleave arg-names (nils->any? arg-specs)))]
          (if varargs
            `(s/cat ~@specced-args :vararg (s/? ~(nil->any? vararg-spec)))
            `(s/cat ~@specced-args)))))
    :arity-n
    (let [{:keys [bodies]} (last fn-tail)
          params (map :params bodies)
          arg-lists (map :args params)
          vararg-lists (map :varargs params)
          arg-specs (map (fn [arglist]
                           (map (fn [arg]
                                  (get-in (last arg) [:annotation :spec]))
                                arglist)) arg-lists)
          vararg-specs (map (fn [vararg-list]
                              (map (fn [vararg]
                                     (-> vararg :form last :annotation :spec)) vararg-list)) vararg-lists)]
      ;; If no arguments are specced, return nil
      (when (some identity (conj (flatten arg-specs) (flatten vararg-specs)))
        `(s/or
          ~@(interleave (arity-labels)
                        (map (fn [arg-list vararg]
                               (let [arg-specs (into [] (comp (map #(get-in (last %) [:annotation :spec]))
                                                              (map nil->any?)) arg-list)
                                     vararg-spec (-> vararg
                                                     :form
                                                     last
                                                     :annotation
                                                     :spec)
                                     arg-names (gen-argument-keys arg-list)]
                                 (let [specced-args (vec (interleave arg-names arg-specs))]
                                   (if vararg
                                     `(s/cat ~@specced-args :varargs (s/? ~(nil->any? vararg-spec)))
                                     `(s/cat ~@specced-args)))))
                             arg-lists vararg-lists)))))))

(combine-arg-specs '{:fn-name a,
                     :ret-annotation {:spec-literal :-, :spec map?},
                     :fn-tail
                     [:arity-1
                      {:params
                       {:args
                        [[:local-symbol
                          {:local-name b, :annotation {:spec-literal :-, :spec int?}}]
                         [:local-symbol
                          {:local-name c}]],
                        :varargs
                        {:amp &,
                         :form
                         [:local-symbol
                          {:local-name so-on,
                           :annotation {:spec-literal :-, :spec int?}}]}},
                       :body [:body [(+ b c)]]}]})


(->> '(destructuring-list :- ::int
                          [a :- ::int b :- int? & [c d]]
                          [a b c d])
     (s/conform ::annotated-defn-args)
     annotated-defn->defn
     #_(s/unform ::defn-args))

(->> '(a :- map?
         ([b :- int?] b)
         ([c :- int? d & e] (+ c d)))
     (s/conform ::annotated-defn-args)
     combine-arg-specs)

(->> '(a :- map? [b :- int?] b)
     (s/conform ::annotated-defn-args)
     combine-arg-specs
     #_(s/unform ::annotated-defn-args))

(->> '(a :- map? [b :- int? [c d]] b)
     (s/conform ::annotated-defn-args))

(->> '(a :- map? [{:keys [b c]} :- map?] (+ b c))
     (s/conform ::annotated-defn-args)
     #_(s/unform ::annotated-defn-args))

(->> '(a :- map? [b :- int? c :- int? & so-on] (+ b c))
     (s/conform ::annotated-defn-args)
     combine-arg-specs
     #_(s/unform ::defn-args))

(s/conform ::annotated-defn-args '(a [[b c]] (+ b c)))
