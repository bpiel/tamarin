(ns tamarin.core)

(def t1 {:a {:b [1 2 3] :c "hello"} :d :what?})
(def t2 {:a [1] :b :hi})

(def ^:dynamic *indent-width* 2)
(def ^:dynamic *target-width* 120)
(def ^:dynamic *max-x* 20)
(def ^:dynamic *max-y* 20)
(def ^:dynamic *max-seq-items* 20)

(defn simple-type
  [v]
  (condp #(% %2) v
    nil? nil
    map? :map
    #(= (type %) clojure.lang.MapEntry) :map-entry
    vector? :vector
    list? :list
    seq? :seq
    set? :set
    string? :string
    integer? :int
    float? :float    
    identity (type v)))

(declare do-it)
(declare pass2)

(def type-bound-map
  {:map ["{" "}"]
   :list ["(" ")"]
   :seq ["(" ")"]
   :vector ["[" "]"]
   :map-entry ["" ""]
   :set ["#{" "}"] })

(defn mk-coll-token-map
  [type children depth length multi-line?]
  {:coll true
   :type type 
   :multi-line? multi-line?
   :length length
   :children children
   :depth depth})

(defn calc-single-line-indent
  ([type]
   (->> type
        type-bound-map
        (map count)
        (apply +)))
  ([init add]
   (+ init 1 (:length add))))

(defn calc-multi-line-indent
  ([type] (calc-single-line-indent type)))

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
  [type coll depth options]
  (let [ml-len (calc-multi-line-indent type)]
    (loop [[head & tail] (take *max-seq-items* coll)
           kids []
           c-depth nil
           sl-len (calc-single-line-indent type)]
      (let [multi-line? (< depth (or c-depth 0))]
        (if (nil? head)
          [(or c-depth (collapse-depth options))
           (if multi-line?
             (mk-coll-token-map type kids depth ml-len true)
             (mk-coll-token-map type kids depth sl-len false))]
          (let [[c-depth' new-kid] (do-it head
                                          (inc depth)
                                          (mk-options options
                                                      (if multi-line?
                                                        ml-len sl-len)
                                                      ml-len))]
            (recur tail
                   (conj kids new-kid)
                   (min (inc depth) (max (or c-depth 0) c-depth'))
                   (calc-single-line-indent sl-len new-kid))))))))

(defn do-scalar
  [v options]
  (let [s (pr-str v)]
    [(collapse-depth options)
     {:coll false
      :multi-line? false
      :length (count s)
      :string s}]))

(defn map->vec
  [v]
  (if (map? v)
    (into [] v)
    v))

(defn do-it
  [v depth options]
  (if (coll? v)
    (do-coll (simple-type v)
             (map->vec v)
             depth
             options)
    (do-scalar v options)))


(defn interleave-delim
  [coll delim]
  (drop-last (interleave coll (repeat delim))))

(defn mk-line
  [coll]
  (let [kids (->> coll
                  :children
                  (map pass2))
        [opener closer] (-> coll :type type-bound-map)
        delim (if (:multi-line? coll)
                {:string "\n" :height 1 :length 0 :line-break true}
                {:string " " :length 1})]
    (flatten
     (concat
      [{:p :open :string opener :length (count opener) :indent (count opener)}]
      (interleave-delim  kids delim)
      [{:p :close :string closer :length (count closer) :indent (count opener)}]))))

(defn pass2
  [v]
  (if (:coll v)
    (mk-line v)
    v))

(defn spaces [n] (apply str (repeat n " ")))

(defn pass3
  "Add indentation"
  [coll init-indent]
  (loop [[{:keys [p indent line-break] :as head} & tail] coll
         indent- init-indent
         agg []]
    (if (nil? head)
      agg
      (let [indent' (cond (= p :open) (+ indent- indent)
                          (= p :close) (- indent- indent)
                          :else indent-)
            agg' (if line-break
                   (conj agg head {:string (spaces indent') :length indent'})
                   (conj agg head))]
        (recur tail
               indent'
               agg')))))

(defn pass4
  [coll]
  (apply str (map :string coll)))


(def vvv {:a #{:d :e} :b (range) :c [1 2 3 4 5 6 7 8 9 10]})

(println (pass4 (pass3 (pass2 (second (do-it vvv 0 [])))
                       0)))

#_ (println (pass4 (pass3 (pass2 {:coll true :multi-line? true :children [{:string "1" :length 1}
                                                                       {:string "2" :length 1}
                                                                       {:string "3" :length 1}]})
                       0)))



