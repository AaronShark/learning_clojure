(ns learning-clojure.recipe1
  (:require [clojure.set :as cset]))

(defn expand
  [the-vector distance length]
  (let [end (count the-vector)
        start (- end distance)
        pattern (subvec the-vector start end)]
    (into [] (take length (cycle pattern)))))

(defn un-LZ777
  [bytes]
  (loop [result []
         remaining bytes]
    (if (seq remaining)
      (let [current (first remaining)
            the-rest (rest remaining)]
        (if-not (vector? current)
          (recur (conj result current) the-rest)
          (recur (into result
                       (expand result (current 0) (current 1)))
                 the-rest)))
      result)))

(defn all-subvecs-from-beginning
  [v]
  (set (map #(subvec v 0 %)
            (range 1 (inc (count v))))))

(defn all-subvecs
  [v]
  (loop [result #{}
         remaining v]
    (if (seq remaining)
      (recur (into result
                   (all-subvecs-from-beginning remaining))
             (into [] (rest remaining)))
      result)))

(defn longest-match-w-beginning
  [left-array right-array]
  (let [all-left-chunks (all-subvecs left-array)
        all-right-chunks-from-beginning (all-subvecs-from-beginning right-array)
        all-matches (cset/intersection all-right-chunks-from-beginning all-left-chunks)]
    (->> all-matches
         (sort-by count >)
         first)))

(defn pos-of-subvec
  [sv v]
  {:pre [(<= (count sv) (count v))]}
  (loop
   [cursor 0]
    (if (or (empty? v) (empty? sv) (= cursor (count v)))
      nil
      (if (= (subvec v cursor (+ (count sv) cursor)) sv)
        cursor
        (recur (inc cursor))))))


(defn LZ77-STEP
  [window look-ahead]
  (let [longest (longest-match-w-beginning window look-ahead)]
    (if-let [pos-subv-w (pos-of-subvec longest window)]
      (let [distance (- (count window) pos-subv-w)
            pos-subv-l (pos-of-subvec longest look-ahead)
            the-char (first (subvec look-ahead (+ pos-subv-l (count longest))))]
        {:distance distance
         :length (count longest)
         :char the-char})
      {:distance 0
       :length 0
       :char (first look-ahead)})))

(defn LZ77
  [bytes-array window-size]
  (->> (loop [result []
              cursor 0
              window []
              look-ahead bytes-array]
         (if (empty? look-ahead)
           result
           (let [this-step-output (LZ77-STEP window look-ahead)
                 distance (:distance this-step-output)
                 length (:length this-step-output)
                 literal (:char this-step-output)
                 raw-new-cursor (+ cursor length 1)
                 new-cursor (min raw-new-cursor (count bytes-array))
                 new-window (subvec bytes-array
                                    #_(max 0 (inc (- new-cursor window-size)))
                                    #_这里应该是个bug，把window变小了1
                                    (max 0 (- new-cursor window-size))
                                    new-cursor)
                 new-look-ahead (subvec bytes-array new-cursor)]
             (recur (conj result
                          [distance length]
                          literal)
                    new-cursor
                    new-window
                    new-look-ahead))))
       (filter (partial not= [0 0]))
       (filter (comp not nil?))
       (into [])))

;;这个算法有个corner case：因为每次找到符合的pattern之后，跳过一个位置
;;不能识别下一位置开头的pattern了。比如下面这个最后的df
;; (LZ77 ["a" "b" "c" "d" "f" "a" "b" "d" "f"] 5)
;; => ["a" "b" "c" "d" "f" [5 2] "d" [4 1]]

;;(un-LZ777 ["a" "b" "c" "f" [4 3] "d"])
;; => ["a" "b" "c" "f" "a" "b" "c" "d"]
