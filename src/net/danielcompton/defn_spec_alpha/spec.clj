(ns net.danielcompton.defn-spec-alpha.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as specs]))

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

(defn annotated-defn-unformer [a]
  (if (and (coll? a) (= :- (first (first (rest a)))))
    (concat (take 1 a) (first (rest a)) (rest (rest a)))
    a))

(s/def ::local-name (s/and simple-symbol? #(not= '& %)))

(s/def ::annotated-map-binding-form (s/merge ::specs/map-bindings ::specs/map-special-binding))

(s/def ::annotated-binding-form
  (s/alt :local-symbol (s/cat :local-name ::local-name
                             :annotation (s/? (s/cat :spec-literal #{:-} :spec any?)))
         :seq-destructure (s/cat :seq-binding-form ::seq-binding-form
                                 :annotation (s/? (s/cat :spec-literal #{:-} :spec any?)))
         :map-destructure (s/cat :map-binding-form ::specs/map-binding-form
                                 :annotation (s/? (s/cat :spec-literal #{:-} :spec any?)))))

(s/def ::annotated-param-list
  (s/and
   vector?
   (s/conformer identity arg-list-unformer)
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



(->> '(a :- map? [b :- int?] b)
     (s/conform ::annotated-defn-args)
     #_(s/unform ::annotated-defn-args))

(->> '(a :- map? [b :- int?] b)
     (s/conform ::annotated-defn-args))

(->> '(a :- map? [{:keys [b c]} :- map?] (+ b c))
     (s/conform ::annotated-defn-args))
