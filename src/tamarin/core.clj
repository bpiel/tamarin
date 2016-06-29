(ns tamarin.core)

(def t1 {:a {:b [1 2 3] :c "hello"} :d :what?})
(def t2 {:a [1] :b :hi})

(def *indent-width* 2)

(defmulti annotate (fn [v _] (type v)))

(defmethod annotate clojure.lang.PersistentArrayMap
  [v depth]
  (let [kids (mapcat #(annotate % (inc depth))
                     v)
        kids-ml? (some :multi-line kids)
        kids-len (+ (* depth *indent-width*)
                    (count kids)
                    (count (map :string kids)))]
    (concat [{:type (type v) :place :open}]
            (mapcat #(annotate % (inc depth))
                    v)
            [{:type (type v) :place :close}])))

(defmethod annotate :default
  [v depth]
  [{:type (type v)
    :string (pr-str v)
    :multi-line false}])

(defn ->annotated
  [v]
  (annotate v 0))


(clojure.pprint/pprint (->annotated t2))

(->annotated t2)









































