(ns ws-ldn-10.ex03
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.math.noise :as noise]
   [thi.ng.color.core :as col]
   [thi.ng.color.gradients :as grad]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.svg.core :as svg]
   [piksel.core :as pix]))

(def octave-pow2 [1.0 2.0 4.0 8.0 16.0 32.0 64.0])

(defn noise-in-octave
  [x y s o]
  (let [o (nth octave-pow2 o)
        s (* s o)]
    (/ (noise/noise2 (* x s) (* y s)) o)))

(defn octave-noise2
  [x y s o]
  (loop [n 0.0, o o]
    (if (>= o 0)
      (recur (+ n (noise-in-octave x y s o)) (dec o))
      n)))

(defn noise-image-gray
  [width octaves scale]
  (let [img (pix/make-image width width)
        pixels (pix/get-pixels img)]
    (doseq [y (range width)
            x (range width)
            :let [n (octave-noise2 x y scale octaves)
                  n (+ (* n 127) 128)]]
      (pix/set-pixel pixels x y width n n n 1))
    (pix/set-pixels img pixels)
    (pix/save-png "noise-gray.png" img)))

(comment

  (time (noise-image-gray 300 6 0.01))
  )

(defn noise-image-gray-turb
  [width octaves scale turbulence]
  (let [img    (pix/make-image width width)
        pixels (pix/get-pixels img)]
    (doseq [y (range width)
            x (range width)
            :let [x' (+ x (turbulence x y))
                  y' (+ y (turbulence y x))
                  n  (octave-noise2 x' y' scale octaves)
                  n  (+ (* n 127) 128)]]
      (pix/set-pixel pixels x y width n n n 1))
    (pix/set-pixels img pixels)
    (pix/save-png "noise-gray-turb.png" img)))

(comment

  (time (noise-image-gray-turb 300 4 0.01 #(* 80 (octave-noise2 % %2 0.02 4))))
  (time (noise-image-gray-turb 300 4 0.02 #(* 80 (octave-noise2 % %2 0.06 4))))
  (time (noise-image-gray-turb
         300 4 0.02
         #(* 80 (* (Math/sin (* % 0.04)) (Math/sin (* %2 0.04))))))
  )

(defn noise-image-color-turb
  [width octaves scale turbulence tonemap]
  (let [img          (pix/make-image width width)
        ^ints pixels (pix/get-pixels img)]
    (doseq [y (range width)
            x (range width)
            :let [x'   (+ x (turbulence x y))
                  y'   (+ y (turbulence y x))
                  n    (octave-noise2 x' y' scale octaves)
                  argb @(col/as-int32 (tonemap (* (+ (* n 0.5) 0.5))))]]
      (aset-int pixels (+ (* y width) x) (unchecked-int argb)))
    (pix/set-pixels img pixels)
    (pix/save-png "noise-col-turb.png" img)))

(defn cosine-gradient
  [id]
  (let [cols (grad/cosine-gradient 256 (grad/cosine-schemes id))]
    (fn [x] (nth cols (m/clamp (* x 255) 0 255)))))

(comment

  (time (noise-image-color-turb
         600 1 0.01
         #(* 80 (octave-noise2 % %2 0.02 4))
         (cosine-gradient :orange-blue)))
  )

(defn noise-lines
  [{:keys [width octaves scale turbulence tonemap bg res len]}]
  (->> (for [y (range 0 width res)
             x (range 0 width res)
             :let [x'    (+ x (turbulence x y))
                   y'    (+ y (turbulence y x))
                   n     (octave-noise2 x' y' scale octaves)
                   theta (* m/PI n)]]
         (svg/line [x y] (m/+ (g/as-cartesian (v/vec2 len theta)) x y)
                   {:stroke (col/as-css (tonemap (* (+ (* n 0.5) 0.5))))}))
       (svg/svg
        {:width width :height width}
        (if bg (svg/rect [0 0] width width {:fill bg})))
       (svg/serialize)))

(comment

  (->> (noise-lines
        {:width      600
         :octaves    1
         :scale      0.01
         :res        4
         :len        50
         :turbulence (constantly 0)
         :tonemap    (cosine-gradient :blue-magenta-orange)
         :bg         (col/rgba 1 0 0.5)})
       (spit "noise-lines.svg")
       (time))
  )
