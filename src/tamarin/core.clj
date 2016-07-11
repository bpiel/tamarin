(ns tamarin.core
  (require [clojure.zip :as z]))

(def t1 {:a {:b [1 2 3] :c "hello"} :d :what?})
(def t2 {:a [1] :b :hi})

(def ^:dynamic *indent-width* 2)
(def ^:dynamic *max-x* 80)
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
    symbol? :symbol
    integer? :int
    float? :float
    (some-fn true? false?) :boolean
    identity (type v)))

(defn map->vec
  [v]
  (if (map? v)
    (into [] v)
    v))

(declare pass1)
(declare pass2)

(def truncator {:string "..." :length 3 :truncator true :type :truncator})

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
  [type children depth length multi-line? trunc?]
  (let [[opener closer] (type type-bound-map)]
    {:coll? true
     :type type 
     :multi-line? multi-line?
     :trunc? trunc?
     :length length
     :children (concat children (when trunc? [truncator]))
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
  (let [ml-len (calc-multi-line-indent type)
        trunc? (->> coll (take (inc *max-seq-items*)) count (< *max-seq-items*))]
    (loop [[head & tail] (take *max-seq-items* coll)
           kids []
           c-depth nil
           sl-len (calc-single-line-indent type)]
      (let [multi-line? (and (not= type :map-entry)
                             (< depth (or c-depth 0)))]
        (if (nil? head)
          [(or c-depth (collapse-depth options))
           (if multi-line?
             (mk-coll-pass1-token-map type kids depth ml-len true trunc?)
             (mk-coll-pass1-token-map type kids depth sl-len false trunc?))]
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

(defn increment-position
  [more? multi-line? base-column line column pos]
  (if more?
    (if multi-line?
      [(inc line) base-column (inc pos)]
      [line (inc column) (inc pos)])
    [line column pos]))

(defn pass2-scalar
  [v line column pos]
  (let [end (+ pos (:length v))
        end-column (+ column (:length v))]
    [(assoc v
            :start-line line
            :start-column column
            :end-line line
            :end-column end-column
            :start pos
            :end end)
     line end-column end]))

(defn pass2-coll
  [coll line column pos]
  (let [[opener closer] (:bounds coll)]
    (loop [[head & tail] (:children coll)
           kids []
           line' line
           column' (-> opener :length (+ column))
           pos' (-> opener :length (+ pos))
           trunc? false]
      (if (nil? head)
        (let [column'' (-> closer :length (+ column'))
              pos'' (-> closer :length (+ pos'))]
          [(-> coll
               (update-in [:bounds 0] assoc
                          :start pos
                          :end (-> opener :length (+ pos))
                          :start-line line
                          :end-line line
                          :start-column column
                          :end-column (-> opener :length (+ column)))
               (update-in [:bounds 1] assoc
                          :start pos'
                          :end pos''
                          :start-line line'
                          :end-line line'
                          :start-column column'
                          :end-column column'')
               (assoc :children kids))
           line' column'' pos''])
        (let [trunc? (> line' *max-y*)
              head' (if trunc? truncator head)
              [new-kid line'' column'' pos''] (pass2 head' line' column' pos')
              [line''' column''' pos'''] (increment-position (-> tail empty? not)
                                                             (:multi-line? coll)
                                                             column'
                                                             line'' column'' pos'')]
          (recur tail
                 (conj kids new-kid)
                 line''' column''' pos''' trunc?))))))

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

(declare pass4)

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
        diff-cols (if (> diff-lines 0)
                    next-column
                    (- next-column column))]
    (concat (repeat diff-lines {:string "\n" :length 1 :line-break true})
            (repeat diff-cols {:string " " :length 1}))))

(defn pass5
  [[head & tail] line column]
  (if head
    (concat (mk-whitespace-token line column (:start-line head) (:start-column head))
            [head]
            (pass5 tail (:end-line head) (:end-column head)))
    []))

(defn render-tokens
  [v]
  (-> v
      (pass1 0 [])
      second
      (pass2 0 0 0)
      first
      pass3
      pass4
      (pass5 0 0)))

(defn pprint-str
  [v]
  (->> v
       render-tokens
       (map :string)
       (apply str)))

#_ (do
     (def vvv [:a :b [:c :d :e :f {:g [1 2 3] :h (range)}]])

     (def zz (pass3 (first (pass2  (second (pass1 vvv 0 []))
                                   0 0 0))))

     (def tokens (pass5 (pass4 zz) 0 0)))

#_ (println (pass4 (pass3 (pass2 {:coll true :multi-line? true :children [{:string "1" :length 1}
                                                                          {:string "2" :length 1}
                                                                          {:string "3" :length 1}]})
                          )))








