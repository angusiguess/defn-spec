(ns net.danielcompton.defn-spec-alpha.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as specs]))

(s/def ::spec-literal #{:-})

(s/def ::spec (s/or :qualified-keyword qualified-keyword?
                    :predicate-symbol symbol?
                    :spec-regex s/regex?
                    :anon-fn list?))

(s/def ::ret-annotation
  (s/cat :spec-literal ::spec-literal :spec ::spec))

(s/def ::binding-form
  (s/alt :local-symbol (s/cat :symbol ::specs/local-name
                              :annotation (s/? (s/cat :spec-literal ::spec-literal
                                                      :spec ::spec)))
         :map-destructure (s/cat :destructure-form ::specs/map-binding-form
                                 :annotation (s/? (s/cat :spec-literal ::spec-literal
                                                         :spec ::spec)))
         :seq-destructure ::specs/seq-binding-form))

(s/def ::param-list
  (s/and
    vector?
    (s/cat :params (s/* ::binding-form)
           :var-params (s/? (s/cat :ampersand #{'&} :var-form ::binding-form)))))

(s/explain ::param-list '[a])

(s/def ::params+body
  (s/cat :params ::param-list
         :body (s/alt :prepost+body (s/cat :prepost map?
                                           :body (s/+ any?))
                      :body (s/* any?))))


;; cribbed from https://github.com/clojure/core.specs.alpha/blob/master/src/main/clojure/clojure/core/specs/alpha.clj#L85
(s/def ::defn-args
  (s/cat :fn-name simple-symbol?
         :ret-annotation (s/? ::ret-annotation)
         :docstring (s/? string?)
         :meta (s/? map?)
         :fn-tail (s/alt :arity-1 ::params+body
                         :arity-n (s/cat :bodies (s/+ (s/spec ::params+body))
                                         :attr-map (s/? map?)))))
