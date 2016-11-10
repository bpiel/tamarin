(ns tamarin.core
  (:require [clojure.zip :as z]))

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
    record? :record
    map? :map
    #(isa? (type %) clojure.lang.MapEntry) :map-entry
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
    identity :other))

(defn get-type
  [v]
  [(simple-type v) (type v)])

(defn map->vec
  [v]
  (if (map? v)
    (into [] v)
    v))

(declare pass1)
(declare pass2)

(def truncator {:string "..." :length 3 :truncator true :type [:truncator nil]})

(def type-bound-map
  {:map ["{" "}"]
   :list ["(" ")"]
   :seq ["(" ")"]
   :vector ["[" "]"]
   :map-entry ["" ""]
   :set ["#{" "}"] })

(defn get-bound-strings
  [[simple-type full-type]]
  (or (simple-type type-bound-map)
      (when (= simple-type :record)
        [(format "#%s{" (pr-str full-type)) "}"])
      ["?<" ">?"]))

(defn calc-single-line-indent
  ([type]
   (->> type
        get-bound-strings
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
  (let [[opener closer] (get-bound-strings type)]
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
      :type (get-type v)
      :length (count s)
      :string s}]))

(defn pass1-coll
  [type coll depth options]
  (let [ml-len (calc-multi-line-indent type)
        trunc? (->> coll (take (inc *max-seq-items*)) count (< *max-seq-items*))
        coll-EOL (concat (take *max-seq-items* coll) [::EOL])]
    (loop [[head & tail] coll-EOL
           kids []
           c-depth nil
           sl-len (calc-single-line-indent type)]
      (let [multi-line? (and (not= (first type) :map-entry)
                             (< depth (or c-depth 0)))]
        (if (= ::EOL head)
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
    (pass1-coll (get-type v)
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
      [(inc line) base-column (+ base-column pos 1)]
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
  (let [[opener closer] (:bounds coll)
        children-EOL (concat (:children coll) [::EOL])]
    (loop [[head & tail] children-EOL
           kids []
           line' line
           column' (-> opener :length (+ column))
           pos' (-> opener :length (+ pos))
           trunc? false]
      (if (= ::EOL head)
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
              [line''' column''' pos'''] (increment-position (-> tail first (not= ::EOL)) 
                                                             (:multi-line? coll)
                                                             column'
                                                             line'' column'' pos'')]
          (recur (if trunc? [::EOL] tail)
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

(defn attach-kids-to-parent
  [node kids]
  (if (not= kids '(nil)) ;; necessary for some reason
    (assoc node :children kids)
    node))

(defn zipper-visit-all
  [zipr f]
  (loop [z' zipr]
    (if (-> z' z/end?)
      z'
      (recur (z/next (z/edit z' f z'))))))

(defn pass3
  [v]
  (let [zipr (z/zipper :coll? :children attach-kids-to-parent v)]
    (-> zipr
        (zipper-visit-all pass3*)
        z/root)))

(declare pass4)

(defn pass4-coll
  [coll]
  (let [[opener closer] (:bounds coll)]
    (doall (concat [opener]
                   (mapcat pass4 (:children coll))
                   [closer]))))

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
     [[head & tail] line column & agg]
     (if head
       (recur tail (:end-line head) (:end-column head)
              (doall (concat agg
                             (mk-whitespace-token line column (:start-line head) (:start-column head))
                             [head])))
       (or agg [])))

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
