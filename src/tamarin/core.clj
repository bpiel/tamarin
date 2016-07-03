(ns tamarin.core)

(def t1 {:a {:b [1 2 3] :c "hello"} :d :what?})
(def t2 {:a [1] :b :hi})

(def ^:dynamic *indent-width* 2)
(def ^:dynamic *target-width* 120)
(def ^:dynamic *max-x* 20)
(def ^:dynamic *max-y* 20)
(def ^:dynamic *max-seq-items* 100)

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

(declare do-it)
(declare pass2)


(defn mk-coll-token-map
  [children depth length multi-line?]
  {:coll true
   :multi-line? multi-line?
   :length length
   :children children
   :depth depth})

(defn calc-single-line-length
  ([] 2)
  ([init add]
   (+ init 1 (:length add))))

(defn calc-multi-line-length
  ([] 2)
  ([init add]
   (max init
        (+ 2 (:length add)))))

(defn collapse-depth
  [options]
  (let [total-single (apply + (map first options))]
    (loop [[[sl ml] & tail] options
           total total-single
           depth 0]
      (if (or (nil? sl)
              (< total *max-x*))
        depth
        (recur tail
               (+ (- total sl) ml)
               (inc depth))))))

(defn mk-options
  [options sl-len ml-len]
  (conj options [sl-len ml-len]))

(defn do-coll
  [coll depth options]
  (loop [[head & tail] coll
         kids []
         c-depth nil
         sl-len (calc-single-line-length)
         ml-len (calc-multi-line-length)]
    (if (nil? head)
      [(or c-depth (collapse-depth options))
       (if (< depth c-depth)
         (mk-coll-token-map kids depth ml-len true)
         (mk-coll-token-map kids depth sl-len false))]
      (let [[c-depth' new-kid] (do-it head
                                      (inc depth)
                                      (mk-options options sl-len ml-len))]
        (recur tail
               (conj kids new-kid)
               (max (or c-depth 0) c-depth')
               (calc-single-line-length sl-len new-kid)
               (calc-multi-line-length ml-len new-kid))))))

(defn do-scalar
  [v options]
  (let [s (pr-str v)]
    [(collapse-depth options)
     {:coll false
      :multi-line? false
      :length (count s)
      :string s}]))

(defn do-it
  [v depth options]
  (if (coll? v)
    (do-coll v depth options)
    (do-scalar v options)))


(defn interleave-delim
  [coll delim]
  (drop-last (interleave coll (repeat delim))))

(defn single-line
  [coll]
  (let [kids (->> coll
                  :children
                  (map pass2))]
    (flatten
     (concat
      [{:p :open :string "[" :length 1}]
      (interleave-delim  kids {:string " " :length 1})
      [{:p :close :string "]" :length 1}]))))

(defn multi-line
  [coll]
  (let [kids (->> coll
                  :children
                  (map pass2))]
    (flatten
     (concat
      [{:p :open :string "[" :length 1}]
      (interleave-delim kids {:string "\n" :height 1 :length 0 :line-break true})
      [{:p :close :string "]" :length 1}]))))

(defn pass2
  [v]
  (if (:coll v)
    (if (:multi-line? v)
      (multi-line v)
      (single-line v))
    v))

(defn spaces [n] (apply str (repeat n " ")))

(defn pass3
  "Add indentation"
  [coll init-indent]
  (loop [[{:keys [p length line-break] :as head} & tail] coll
         indent init-indent
         agg []]
    (if (nil? head)
      agg
      (let [indent' (cond (= p :open) (+ indent length)
                          (= p :close) (- indent length)
                          :else indent)
            agg' (if line-break
                   (conj agg head {:string (spaces indent') :length indent'})
                   (conj agg head))]
        (recur tail
               indent'
               agg')))))

(defn pass4
  [coll]
  (apply str (map :string coll)))


(def vvv [1  [1 2 3 ["hello"] 6 7 8] 6 ])

(println (pass4 (pass3 (pass2 (second (do-it vvv 0 [])))
                       0)))

#_ (println (pass4 (pass3 (pass2 {:coll true :multi-line? true :children [{:string "1" :length 1}
                                                                       {:string "2" :length 1}
                                                                       {:string "3" :length 1}]})
                       0)))
