(ns tamarin.core
  (require [clojure.zip :as z]))

(def t1 {:a {:b [1 2 3] :c "hello"} :d :what?})
(def t2 {:a [1] :b :hi})

(def ^:dynamic *indent-width* 2)
(def ^:dynamic *target-width* 120)
(def ^:dynamic *max-x* 30)
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

(defn map->vec
  [v]
  (if (map? v)
    (into [] v)
    v))

(declare pass1)
(declare pass2)

(def type-bound-map
  {:map ["{" "}"]
   :list ["(" ")"]
   :seq ["(" ")"]
   :vector ["[" "]"]
   :map-entry ["" ""]
   :set ["#{" "}"] })

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

(defn mk-coll-pass1-token-map
  [type children depth length multi-line?]
  (let [[opener closer] (type type-bound-map)]
    {:coll? true
     :type type 
     :multi-line? multi-line?
     :length length
     :children children
     :depth depth
     :bounds [{:boundary :open :string opener :length (count opener)}
              {:boundary :close :string closer :length (count closer)}]}))

(defn pass1-scalar
  [v options]
  (let [s (pr-str v)]
    [(collapse-depth options)
     {:coll? false
      :type (simple-type v)
      :length (count s)
      :string s}]))

(defn pass1-coll
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
             (mk-coll-pass1-token-map type kids depth ml-len true)
             (mk-coll-pass1-token-map type kids depth sl-len false))]
          (let [[c-depth' new-kid] (pass1 head
                                          (inc depth)
                                          (mk-options options
                                                      (if multi-line?
                                                        ml-len sl-len)
                                                      ml-len))]
            (recur tail
                   (conj kids new-kid)
                   (min (inc depth) (max (or c-depth 0) c-depth'))
                   (calc-single-line-indent sl-len new-kid))))))))

(defn pass1
  [v depth options]
  (if (coll? v)
    (pass1-coll (simple-type v)
             (map->vec v)
             depth
             options)
    (pass1-scalar v options)))

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


(defn spaces [n] (apply str (repeat n " ")))

(defn increment-position
  [multi-line? base-column line column pos]
  (if multi-line?
    [(inc line) base-column (inc pos)]
    [line (inc column) (inc pos)]))

(defn pass2-scalar
  [v line column pos]
  (let [end (+ pos (:length v))]
    [(assoc v
            :line line
            :column column
            :start pos
            :end end)
     line column end]))

(defn pass2-coll
  [coll line column pos]
  (let [[opener closer] (:bounds coll)]
    (loop [[head & tail] (:children coll)
           kids []
           line' line
           column' (-> opener :length (+ column))
           pos' (-> opener :length (+ pos))]
      (if (nil? head)
        [(-> coll
             (assoc-in [:bounds 0 :start] pos)
             (assoc-in [:bounds 1 :end] (-> closer :length (+ pos')))
             (assoc :children kids))
         line' column' pos']
        (let [[new-kid line'' column'' pos''] (pass2 head line' column' pos')
              [line''' column''' pos'''] (increment-position (:multi-line? coll)
                                                             column
                                                             line'' column'' pos'')]
          (recur tail
                 (conj kids new-kid)
                 line''' column''' pos'''))))))

(defn pass2
  "Add :line, :col, :start, :end"
  [v line column start]
  (if (:coll? v)
    (pass2-coll v line column start)
    (pass2-scalar v line column start)))

(defn pass3
  [v]
  (z/zipper :coll? :children #(assoc % :children %2) v ))


(def vvv [:a {:b :c} #{ 1 2 3}])

(clojure.pprint/pprint  (pass1 vvv 0 []))

(clojure.pprint/pprint (pass2  (second (pass1 vvv 0 []))
                               0 0 0))

#_ (println (pass4 (pass3 (pass2 {:coll true :multi-line? true :children [{:string "1" :length 1}
                                                                       {:string "2" :length 1}
                                                                       {:string "3" :length 1}]})
                       0)))
 "
## pass1 

{:coll true
   :type type 
   :multi-line? multi-line?
   :length length
   :children children
   :depth depth
   :bounds [{:length 1 :string '['} ...]} 

{:coll false
      :length (count s)
      :string s}


## pass2

:line
:col
:start
:end

## pass3

attach zipper

## pass4

token stream

## pass5

rendered streams


{

"








