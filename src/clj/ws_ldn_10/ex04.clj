(ns ws-ldn-10.ex04
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.color.core :as col]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.svg.core :as svg]))

(defn make-dipole
  [width charge]
  {:pos (v/vec2 (m/random width) (m/random width))
   :charge (if (odd? (rand-int 2)) charge (- charge))})

(defn make-agent
  [pole theta speed]
  {:path        [(:pos pole)]
   :pos         (m/+ (:pos pole) (g/as-cartesian (v/vec2 speed theta)))
   :speed       speed
   :parent      pole
   :target-dist (Math/pow (* speed 0.9) 2)
   :active      true})

(defn update-agent
  [poles bounds {:keys [pos target-dist parent] :as a}]
  (if (:active a)
    (let [a (reduce
             (fn [a pole]
               (let [delta (m/- pos (:pos pole))
                     dist  (m/mag-squared delta)]
                 (if (< dist target-dist)
                   (reduced (assoc a :active false :target (:pos pole)))
                   (if (not= pole parent)
                     (update a :dir m/+ (m/* delta (/ (:charge pole) dist)))
                     (update a :dir m/+ (m/* delta (Math/abs (/ (:charge pole) dist))))))))
             (assoc a :dir (v/vec2))
             poles)]
      (if (:active a)
        (let [pos' (m/+ (:pos a) (m/normalize (:dir a) (:speed a)))]
          (-> a
              (update :path conj (:pos a))
              (assoc :pos    pos'
                     :active (g/contains-point? bounds pos'))))
        (if-let [t (:target a)]
          (update a :path conj t)
          a)))
    a))

(defn field-lines
  [{:keys [width nump numa speed charge iter]}]
  (let [bounds (r/rect width)
        poles  (repeatedly nump #(make-dipole width charge))
        _ (prn poles)
        agents (mapcat
                (fn [p]
                  (map #(make-agent p % speed)
                       (butlast (m/norm-range numa))))
                poles)
        agents (reduce
                (fn [agents _]
                  (prn _)
                  (mapv (fn [a] (update-agent poles bounds a)) agents))
                agents
                (range iter))]
    (->> agents
         (map #(svg/line-strip (:path %)))
         (svg/group {:stroke "blue"})
         (svg/svg
          {:width width :height width})
         (svg/serialize))))

(comment

  (spit "agents.svg"
        (field-lines
         {:width 600
          :nump  10
          :numa  10
          :speed 3
          :charge 5
          :iter 1000}))
  )
