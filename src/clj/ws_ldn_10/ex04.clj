(ns ws-ldn-10.ex04
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.color.core :as col]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.svg.core :as svg]))

(def self-avoid false)
(def max-dist (Math/pow 60 2))
(def max-charge 5)

(defn normalize-charge
  [ch] (+ (/ ch (* 2 max-charge)) 0.5))

(defn make-dipole
  [width charge]
  {:pos (v/vec2 (m/random width) (m/random width))
   :charge (if (pos? (rand-int 2)) charge (- charge))})

(defn make-agent
  [pole theta speed]
  {:path        [(:pos pole)]
   :charges     [(normalize-charge (:charge pole))]
   :pos         (m/+ (:pos pole) (g/as-cartesian (v/vec2 speed (* theta m/TWO_PI))))
   :speed       speed
   :parent      pole
   :target-dist (Math/pow (* speed 0.9) 2)
   :active      (pos? (:charge pole))})

(defn update-agent
  [poles bounds {:keys [pos target-dist parent] :as a}]
  (if (:active a)
    (let [a (reduce
             (fn [a pole]
               (let [delta   (m/- pos (:pos pole))
                     dist    (m/mag-squared delta)
                     charge' (/ (:charge pole) dist)
                     a       (if (< dist max-dist)
                               (-> a
                                   (update :charge + (* (:charge pole)
                                                        (- 1 (min 1 (/ dist max-dist)))))
                                   (update :num-charges inc))
                               a)]
                 (if (< dist target-dist)
                   (reduced (assoc a :active false :target pole))
                   (if (or (not= pole parent) (not self-avoid))
                     (update a :dir m/+ (m/* delta charge'))
                     (update a :dir m/+ (m/* delta charge'))))))
             (assoc a :dir (v/vec2) :charge 0 :num-charges 0)
             poles)
          a (if (pos? (:num-charges a))
              (update a :charge / (:num-charges a))
              a)]
      (if (:active a)
        (let [pos' (m/+ (:pos a) (m/normalize (:dir a) (:speed a)))]
          (-> a
              (update :path conj (:pos a))
              (update :charges conj (normalize-charge (:charge a)))
              (assoc :pos    pos'
                     :active (g/contains-point? bounds pos'))))
        (if-let [t (:target a)]
          (-> a
              (update :path conj (:pos t))
              (update :charges conj (normalize-charge (:charge t))))
          a))
      a))

  (defn field-line
    [a]
    (->> (map vector (:path a) (:charges a))
         (partition 2 1)
         (map
          (fn [[[p ch] [q]]]
            (svg/line p q {:stroke (m/mix (col/rgba 0 0 1) (col/rgba 1 0 0) ch)}))))))

(defn export-frame
  [frame width agents]
  (println "writing frame:" frame)
  (->> agents
       (map field-line)
       (svg/svg {:width width :height width})
       (svg/serialize)
       (spit (format "assets/agents-%04d.svg" frame))))

(defn field-lines
  [{:keys [width nump numa speed charge iter]}]
  (let [bounds (r/rect width)
        poles  (repeatedly nump #(make-dipole width charge))
        agents (mapcat
                (fn [p]
                  (map #(make-agent p % speed)
                       (butlast (m/norm-range numa))))
                poles)
        agents (loop [i 0, agents agents]
                 (if (< i iter)
                   (let [agents (mapv #(update-agent poles bounds %) agents)]
                     (export-frame i width agents)
                     (recur (inc i) agents))
                   agents))]
    (println "done")))

(comment

  (field-lines
   {:width  600
    :nump   20
    :numa   10
    :speed  4
    :charge max-charge
    :iter   150})

  )
