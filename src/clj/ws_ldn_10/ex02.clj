(ns ws-ldn-10.ex02
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.color.core :as col]
   [piksel.core :as pix]))

(defn svg-doc
  [width body]
  (->> body
       (svg/svg {:width width :height width})
       (svg/serialize)))

(defn compute-dejong
  "Computes a single DeJong 2d point vector for given params and XY pos"
  [a b c d x y]
  (v/vec2
   (+ (Math/sin (* a y)) (Math/cos (* (* b x) x)))
   (+ (Math/sin (* (* c x) x)) (Math/cos (* d y)))))

(defn dejong-svg
  [{:keys [width iter a b c d color bg]}]
  (let [scale  (/ width 4)
        center (v/vec2 (/ width 2))]
    (->> (range iter)
         ;; iterative system: f(x+1) = f(f(x))
         (reduce
          (fn [[points [x y]] _]
            (let [pos (compute-dejong a b c d x y)]
              [(conj points (svg/circle (m/madd pos scale center) 1)) pos]))
          ['() [(m/random width) (m/random width)]])
         (first)
         (svg/group
          {:fill (or color "rgba(0,0,0,0.25)") :stroke "none"}
          (if bg (svg/rect [0 0] width width {:fill bg})))
         (svg-doc width))))

(comment

  (spit "dejong.svg"
        (dejong-svg
         {:width 600
          :iter  100000
          :a     (m/random -3 3)
          :b     (m/random -3 3)
          :c     (m/random -3 3)
          :d     (m/random -3 3)}))

  )

(defn dejong-bitmap
  [{:keys [width iter a b c d color bg blend]}]
  (prn :coeffs a b c d)
  (let [scale            (/ width 4.1)
        center           (v/vec2 (/ width 2))
        img              (pix/make-image width width)
        pixels           (pix/get-pixels img)
        [red green blue] (map #(* % 255) @color)
        alpha            (peek @color)
        blend-fn         (pix/blend-modes blend)]
    (when bg
      (apply pix/fill-array pixels @bg))
    (reduce
     (fn [[x y] _]
       (let [pos   (compute-dejong a b c d x y)
             [x y] (m/madd pos scale center)]
         (pix/blend-pixel pixels x y width red green blue alpha blend-fn)
         pos))
     [(m/random width) (m/random width)]
     (range iter))
    (pix/set-pixels img pixels)
    (pix/save-png "dejong.png" img)))

(comment

  (dejong-bitmap
   {:width 600
    :iter  1000000
    :a     (m/random -3 3)
    :b     (m/random -3 3)
    :c     (m/random -3 3)
    :d     (m/random -3 3)
    :color (col/rgba 0.6 0.3 0.1 0.15)
    :bg    (col/rgba 0 0 0.1)
    :blend :add})

  (dejong-bitmap
   {:width 1280
    :iter  3000000
    :a     1.815
    :b     -1.687
    :c     2.551
    :d     1.151
    :color (col/rgba 0.7 0.2 0.25 0.15)
    :bg    (col/rgba 0 0 0.1)
    :blend :add})
  
  ;; -2.726 -2.082 -2.239 -2.340
  )

(defn dejong-bitmap-duo
  [{:keys [width iter a b c d col1 col2 alpha bg blend]}]
  (prn :coeffs a b c d)
  (let [scale      (/ width 4.1)
        center     (v/vec2 (/ width 2))
        img        (pix/make-image width width)
        pixels     (pix/get-pixels img)
        [r1 g1 b1] (map #(* % 255) @col1)
        [r2 g2 b2] (map #(* % 255) @col2)
        blend-fn   (pix/blend-modes blend)]
    (when bg
      (apply pix/fill-array pixels @bg))
    (reduce
     (fn [[x y] _]
       (let [pos   (compute-dejong a b c d x y)
             red   (m/mix* r1 r2 (+ (* x 0.25) 0.5))
             green (m/mix* g1 g2 (+ (* x 0.25) 0.5))
             blue  (m/mix* b1 b2 (+ (* y 0.25) 0.5))
             [x y] (m/madd pos scale center)]
         (pix/blend-pixel pixels x y width red green blue alpha blend-fn)
         pos))
     [(m/random width) (m/random width)]
     (range iter))
    (pix/set-pixels img pixels)
    (pix/save-png "dejong.png" img)))

(comment

  (dejong-bitmap-duo
   {:width 1280
    :iter  3000000
    :a     1.815
    :b     -1.687
    :c     2.551
    :d     1.151
    :col1  (col/rgba 0.7 0.2 0.15)
    :col2  (col/rgba 0.1 0.2 0.5)
    :alpha 0.15
    :bg    (col/rgba 0 0 0.1)
    :blend :add})

  )
