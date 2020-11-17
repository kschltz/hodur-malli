(ns hodur-malli.core
 (:require [clojure.walk :refer [postwalk]]
           [datascript.core :as ds]))


(def malli->hodur-types
 {:map     :map
  string?  'String
  boolean? 'Boolean
  double?  'Float
  float?   'Float
  int?     'Integer
  inst?    'DateTime
  uuid?    'ID})

(def hodur->malli-types
 (->> malli->hodur-types
      (map (comp vec reverse))
      (into {})))

(comment "under development"
         (defn ->hodur
          [schema & {:keys [default]
                     :or   {default {:spec/tag true}}}]
          (let [m (->> schema
                       (tree-seq vector? identity)
                       (filter vector?)
                       (filter (comp #(= :map %) first))
                       (map
                        (fn [[_ {:hodur/keys [name]
                                 :as         opts} & children]]
                         [name [(symbol name)
                                (->> children
                                     (map (fn [[name t]]
                                           (with-meta (symbol name)
                                                      (merge opts
                                                             {:type (malli->hodur-types (cond-> t
                                                                                                (vector? t) first
                                                                                                :else identity))}))))
                                     (into []))]]))
                       (into {}))]
           (->> m
                (postwalk (fn [x]
                           (if (and (symbol? x)
                                    (:type (meta x)))

                            (with-meta x
                                       (update
                                        (meta x)
                                        :type #(if (= :map %)
                                                (first (m (keyword x))) %))) x)))
                vals
                (into [(with-meta 'default default)] cat)
                ))))

(defn enum-registry [metadb]
 (->> metadb
      (ds/q '[:find (pull ?t [*
                              {:field/_parent [:field/kebab-case-name]}])
              :where [?t :type/enum]])
      (mapcat identity)
      (map (fn [{:type/keys [kebab-case-name]
                 children   :field/_parent}]
            [kebab-case-name (->> children
                                  (map (comp name :field/kebab-case-name))
                                  (into [:enum]))]))
      (into {})))


(defn resolve-entities
 [type-name-key field-name-key es]
 (->> es
      (mapcat identity)
      (map (fn [{tn       type-name-key
                 children :field/_parent}]
            [tn (->> children
                     (map (fn [{fname               field-name-key
                                [m n]               :field/cardinality
                                {:type/keys [nature name]
                                 :spec/keys [extend gen]
                                 :as        f-type} :field/type}]
                           (let [t (if (= nature :user)
                                    (get f-type type-name-key)
                                    (or extend (hodur->malli-types
                                                (and name
                                                     (symbol name))) any?))]
                            (->> [fname
                                  ;(if gen {:gen/fmap gen})
                                  (if (not= 1 n) [:sequential t]
                                                 t)]
                                 (filter some?)
                                 (into [])))))
                     (into [:map]))]))
      (into {})))


(def entities-base-clause
 '[:find (pull ?e
               [*
                {:field/_parent
                 [:field/kebab-case-name
                  :field/cardinality
                  {:field/type [*]}]}])])

(defn entities-registry [metadb type-name-key field-name-key]
 (->> metadb
      (ds/q (conj entities-base-clause
                  :where
                  '[?e :type/nature :user]
                  '(not [?e :type/enum])))
      (resolve-entities type-name-key field-name-key)))

(defn base-entities [metadb type-name-key field-name-key]
 (->> metadb
      (ds/q (conj entities-base-clause
                  :where
                  '[?e :type/nature :user]
                  '(not [?f :field/type ?e])
                  '(not [?e :type/enum])))
      (resolve-entities type-name-key field-name-key)))

(defn ->malli
 [metadb & {:keys [type-name-key field-name-key]
            :or   {type-name-key  :type/kebab-case-name
                   field-name-key :field/kebab-case-name}}]
 (let [enums (enum-registry metadb)
       sub-entities (entities-registry metadb type-name-key field-name-key)
       base-entities (base-entities metadb type-name-key field-name-key)]
  (->> base-entities
       (vals)
       (into [:or {:registry (merge enums sub-entities)}]))))

