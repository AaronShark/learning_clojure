(ns learning-clojure.recipe2
  (:import (java.awt image.BufferedImage Color)
           (javax.imageio ImageIO)
           (java.io File)))

(defn pascal-row-step
  [yield pascal-row]
  {:pre [(> (get pascal-row 0) 0)]}
  (let [cnt-elts (count pascal-row)
        half-row (subvec pascal-row 0
                         (inc (double (/ cnt-elts 2))))
        padded-half-row (into [0] half-row)
        half-step (vec (map (comp (partial apply yield) vec)
                            (partition 2 1 padded-half-row)))
        other-half-step (vec (if (even? cnt-elts)
                               (-> half-step
                                   butlast
                                   reverse)
                               (-> half-step
                                   reverse)))]
    (into half-step other-half-step)))

(defn pascal-rows
  "行数从零算起"
  [yield row-number]
  (loop [nb 0
         result []
         latest-result [1]]
    (if (<= nb row-number)
      (recur (inc nb)
             (conj result latest-result)
             (pascal-row-step yield latest-result))
      result)))

(defn even-odd-yield
  [n1 n2]
  (mod (+ n1 n2) 2))

(def gr-triangles (partial pascal-rows even-odd-yield))

(defn draw
  [size]
  (let [img (BufferedImage. size size BufferedImage/TYPE_INT_ARGB)
        plot-rows (gr-triangles size)
        plots (for [x (range 0 size)
                    y (range 0 x)
                    :when (= 1 (get (get plot-rows x) y))]
                [x y])
        gfx (.getGraphics img)]
    (.setColor gfx Color/WHITE)
    (.fillRect gfx 0 0 size size)
    (.setColor gfx Color/BLACK)
    (doseq [p plots]
      (.drawLine gfx
                 (get p 0) (get p 1) (get p 0) (get p 1)))
    (ImageIO/write img "png"
                   (File. "./result.png"))))
