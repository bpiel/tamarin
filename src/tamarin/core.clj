(ns tamarin.core)

(def t1 {:a {:b [1 2 3] :c "hello"} :d :what?})
(def t2 {:a [1] :b :hi})

(def *indent-width* 2)
(def *target-width* 120)
(def *max-height* 20)
(def *max-seq-items* 10)

(declare annotate-all)

(defn simple-type
  [v]
  (condp #(% %2) v
    nil? nil
    map? :map
    vector? :vector
    list? :list
    seq? :seq
    string? :string
    integer? :int
    float? :float    
    identity (type v)))

(def annotate nil) ;; HACK to force redefinition
(defmulti annotate (fn [v _] (simple-type v)))

(defn do-coll-single-line [] nil)

(defn interleave-linebreaks
  [coll]
  (interleave coll (repeat {:height 1 :length 1 :width 0 :string "\n"})))

(defn do-coll-multi-line
  [coll depth start-line]
  (loop [[head & tail] coll ;; take *max*
         total-height 0
         agg []]
    (if (nil? head)
      [(interleave-linebreaks agg) false]
      (let [item (annotate head depth)
            item-truncd? (:truncated item)
            height (:height item)
            new-height (+ height total-height)]
        (if (or item-truncd?
                (> new-height
                   *max-height*))
          [(interleave-linebreaks agg) true]
          (recur tail
                 new-height
                 (conj agg item)))))))

(defn do-coll
  [coll depth start-line]
  (let [[chips tail too-long?] (do-coll-single-line (take *max-seq-items*
                                                          (map #(annotate % depth #_ "laziness is key!")
                                                               coll))
                                                    depth
                                                    start-line)]
    (if too-long?
      (do-coll-multi-line (concat chips
                                  (map #(annotate % depth #_ "laziness is key!"))))
      [chips false])))


(defmethod annotate :map
  [v depth]
  (concat [{:type (simple-type v) :place :open}]
          (mapcat #(annotate % (inc depth))
                  v)
          [{:type (simple-type v) :place :close}]))

(defmethod annotate :default
  [v depth]
  (let [s (pr-str v)
        c (count s)]
    [{:type (simple-type v)
      :string s
      :height 0
      :width c
      :length c
      :multi-line false}]))

(defn annotate-all
  [depth coll]
  (let [kids (mapcat #(annotate % (inc depth))
                     coll)]
    {:kids kids
     :multi-line? (some :multi-line kids)
     :max-width (->> coll
                     (map :width)
                     (apply max))
     :sum-height (->> coll
                      (map :height)
                      (apply +))
     :sum-length (->> coll
                      (map :length)
                      (apply +))}))


(defn ->annotated
  [v]
  (annotate v 0))


(clojure.pprint/pprint (->annotated t2))

(->annotated t2)














