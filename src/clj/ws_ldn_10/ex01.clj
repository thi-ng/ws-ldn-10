(ns ws-ldn-10.ex01
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.polygon :as poly]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.svg.adapter :as svgadapt]
   [thi.ng.color.core :as col]))

(defn svg-doc
  [width body]
  (->> body
       (svg/svg {:width width :height width})
       (svgadapt/all-as-svg)
       (svg/serialize)))

(defn ex01
  [width num r1 r2]
  (->> (for [i (butlast (m/norm-range num))
             :let [pos   (-> (v/vec2 r1 (* m/TWO_PI i))
                             (g/as-cartesian)
                             (m/madd [1 0.5] (/ width 2)))]]
         (c/circle pos r2))
       (svg/group {:stroke "blue" :fill "none"})
       (svg-doc width)))

;; (spit "ex01-200.svg" (ex01 600 100 200 90))

(defn ex02
  [width num r1 shape-fn]
  (->> (for [i (butlast (m/norm-range num))
             :let [theta (* m/TWO_PI i)
                   pos   (-> (v/vec2 r1 theta)
                             (g/as-cartesian)
                             (m/madd [1 0.5] (/ width 2)))]]
         (shape-fn pos theta))
       (svg/group {:stroke "rgba(0,0,255,0.25)" :fill "none"})
       (svg-doc width)))

(defn shape
  [res radius]
  (let [proto (-> (c/circle radius)
                  (g/center)
                  (g/as-polygon res))]
    (fn [pos theta]
      (-> proto
          (g/rotate (* 2 theta))
          (g/translate pos)))))

(defn star-shape
  [res r1 r2]
  (let [proto (poly/cog r2 res [(/ r1 r2) 1])]
    (fn [pos theta]
      (-> proto
          (g/rotate (* 2 theta))
          (g/translate pos)))))

;; (spit "ex02-100.svg" (ex02 600 200 200 (shape 3 90)))

(defn ex03
  [{:keys [width num radius shape bg color]}]
  (->> (for [i (butlast (m/norm-range num))
             :let [theta (* m/TWO_PI i)
                   pos   (-> (v/vec2 (radius theta) theta)
                             (g/as-cartesian)
                             (m/madd [1 1] (/ width 2)))]]
         (shape pos theta))
       (svg/group
        {:stroke (or color "rgba(0,0,255,0.25)") :fill "none"}
        (if bg (svg/rect [0 0] width width {:fill bg})))
       (svg-doc width)))

(defn waveform
  [min max freq]
  (let [offset (m/mix* min max 0.5)
        delta (- max offset)]
    (fn [theta]
      (+ offset (* delta (Math/sin (* freq theta)))))))

(comment
  
  (spit "ex03.svg"
        (ex03 {:width  600
               :num    400
               :radius (waveform 100 200 4)
               :shape  (shape 3 90)}))

  (spit "ex03.svg"
        (ex03 {:width  600
               :num    600
               :radius (waveform 50 200 8)
               :shape  (star-shape 5 25 90)
               :color  "rgba(255,250,240, 0.25)"
               :bg     "#001"})))

(defn ex04
  [{:keys [width num radius shape dist bg color]}]
  (->> (for [i (butlast (m/norm-range num))
             :let [theta (* m/TWO_PI i)
                   pos   (-> (v/vec2 (radius theta) theta)
                             (g/as-cartesian)
                             (m/madd [1 1] (/ width 2)))]]
         (g/sample-uniform (shape pos theta) dist false))
       ;; concatenate all points into single seq
       (mapcat identity)
       ;; represent each as small dot
       (map #(c/circle % 1.5))
       (svg/group
        {:fill (or color "rgba(0,0,255,0.25)") :stroke "none"}
        (if bg (svg/rect [0 0] width width {:fill bg})))
       (svg-doc width)))

(comment

  (spit "ex04.svg"
        (ex04 {:width  600
               :num    1000
               :radius (waveform 50 200 8)
               :dist   40
               :shape  (star-shape 5 25 90)
               :color  (col/hsva 1/6 0.15 1 0.25)
               :bg     "#001"}))
  )
