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
    keyword? :keyword
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
  (let [end (+ pos (:length v))
        column' (+ column (:length v))]
    [(assoc v
            :line line
            :column column
            :start pos
            :end end)
     line column' end]))

(defn pass2-coll
  [coll line column pos]
  (let [[opener closer] (:bounds coll)]
    (loop [[head & tail] (:children coll)
           kids []
           line' line
           column' (-> opener :length (+ column))
           pos' (-> opener :length (+ pos))]
      (if (nil? head)
        (let [column'' (-> closer :length (+ column'))
              pos'' (-> closer :length (+ pos'))]
          [(-> coll
               (update-in [:bounds 0] assoc
                          :start pos
                          :end (-> opener :length (+ pos))
                          :line line
                          :column column)
               (update-in [:bounds 1] assoc
                          :start pos'
                          :end pos''
                          :line line'
                          :column column'')
               (assoc :children kids))
           line' column'' pos''])
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

(defn pass3-scalar
  [node zipr]

  (assoc node :zipper zipr))

(defn pass3-coll
  [node zipr]
  (-> node
      (assoc-in [:bounds 0 :zipper] zipr)
      (assoc-in [:bounds 1 :zipper] zipr)))

(defn pass3*
  [node zipr]
  (cond
    (-> node map? not) node ;; TODO remove this line
    (:coll? node) (pass3-coll node zipr)
    :default (pass3-scalar node zipr)))

(defn zipper-visit-all
  [zipr f & args]
  (loop [z' zipr]
    (if (z/end? z')
      z'
      (recur (z/next (apply z/edit z' f args))))))

(defn pass3
  [v]
  (let [zipr (z/zipper :coll? :children #(assoc % :children %2) v)]
    (-> zipr
        (zipper-visit-all pass3* zipr)
        z/root)))

(defn pass4-coll
  [coll]
  (let [[opener closer] (:bounds coll)]
    (concat [opener]
            (mapcat pass4 (:children coll))
            [closer])))

(defn pass4-scalar [v] [v])

(defn pass4
  [v]
  (if (:coll? v)
    (pass4-coll v)
    (pass4-scalar v)))

(defn mk-whitespace-token
  [line column next-line next-column]
  (let [diff-lines (- next-line line)
        diff-cols (- next-column column)
        s (apply str (concat (when (> diff-lines 0)
                               (repeat diff-lines "\n"))
                             (when (> diff-cols 0)
                               (repeat diff-cols " "))))]
    (when (not-empty s)
      {:string s
       :length (count s)})))

(defn pass5
  [[head & tail] line column]
  (if head
    (concat [(mk-whitespace-token line column (:line head) (:column head))]
            [head]
            (pass5 tail (:line head) (:column head)))
    []))

(def vvv [:a :b [:c]])

(clojure.pprint/pprint  (pass1 vvv 0 []))

(clojure.pprint/pprint (pass2  (second (pass1 vvv 0 []))
                               0 0 0))

(def zz (pass3 (first (pass2  (second (pass1 vvv 0 []))
                              0 0 0))))

(def tokens (pass5 (pass4 zz) 0 0))

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

token stream w/o whitespace

## pass5

token stream w/ whitespace


{

"




