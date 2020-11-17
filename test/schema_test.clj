(ns schema-test
 (:require [clojure.test :refer :all]
           [malli.generator :as mg]
           [malli.core :as m]
           [datascript.core :as ds]
           [hodur-malli.core :as c]
           [hodur-engine.core :as h]))

(def test-schema
  '[^{:spec/tag true}
    default

    SubEntity
    [^String name
     ^Float value]

    ^{:enum true}
    Operation
    [ADD SUBTRACT MULTIPLY DIVIDE]

    MainEntity
    [^String Name
     ^Operation op
     ^{:type SubEntity
      :cardinality [0 n]} children]])


(deftest validation-test
 (is (m/validate (c/->malli
                  @(h/init-schema test-schema)
                  :field-name-key :field/kebab-case-name
                  :type-name-key :type/camelCaseName)
                 {:name "Weapon X"
                  :op "add"
                  :children [{:name "X-21"
                              :value 15.0}]})))
