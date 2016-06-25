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
  [^double ch] (+ (/ ch (* 2.0 max-charge)) 0.5))

(defn make-dipole
  [width charge]
  {:pos    (v/vec2 (m/random width) (m/random width))
   :charge (if (pos? (rand-int 2)) charge (- charge))})

(defprotocol IAgent
  (update-agent [a poles bounds]))

(deftype Agent
    [parent path charges pos speed t-dist active]
  IAgent
  (update-agent
      [a poles bounds]
    (if active
      (loop [dir (v/vec2), charge 0.0, num-charges 0, poles poles]
        (if poles
          (let [pole                (first poles)
                delta               (m/- pos (get pole :pos))
                ^double dist        (m/mag-squared delta)
                ^double p-charge    (get pole :charge)
                p-charge'           (/ p-charge dist)
                [^double charge
                 ^long num-charges] (if (< dist max-dist)
                                      [(+ charge (* p-charge (- 1 (min 1 (/ dist max-dist)))))
                                       (inc num-charges)]
                                      [charge num-charges])]
            (if (> dist t-dist)
              (recur (m/madd delta p-charge' dir)
                     (double charge) (long num-charges)
                     (next poles))
              (Agent.
               parent
               (conj path (get pole :pos))
               (conj charges (normalize-charge (get pole :charge)))
               (get pole :pos)
               speed
               t-dist
               false)))
          (let [pos'   (m/+ pos (m/normalize dir speed))
                charge (if (pos? num-charges) (/ charge num-charges) 0)
                active (g/contains-point? bounds pos')]
            (Agent.
             parent
             (conj path pos)
             (conj charges (normalize-charge charge))
             pos'
             speed
             t-dist
             active))))
      a)))

(defn make-agent
  [pole theta speed]
  (Agent.
   pole
   [(:pos pole)]
   [(normalize-charge (:charge pole))]
   (m/+ (:pos pole) (g/as-cartesian (v/vec2 speed (* theta m/TWO_PI))))
   speed
   (Math/pow (* speed 0.9) 2)
   (pos? (:charge pole))))

(defn field-line
  [^Agent a col1 col2]
  (->> (range (dec (count (.-path a))))
       (map
        (fn [i]
          (svg/line (nth (.-path a) i) (nth (.-path a) (inc i))
                    {:stroke (m/mix color-neg color-pos (nth (.-charges a) i))})))))

(defn export-frame
  [frame width agents col1 col2]
  (println "writing frame:" frame)
  (->> agents
       (mapcat #(field-line % col1 col2))
       (svg/svg {:width width :height width})
       (svg/serialize)
       (spit (format "assets/agents-%04d.svg" frame))))

(defn field-lines
  [{:keys [poles col-neg col-pos width numa speed iter]}]
  (let [bounds (r/rect width)
        agents (mapcat
                (fn [p]
                  (map #(make-agent p % speed) (butlast (m/norm-range numa))))
                (filter #(pos? (:charge %)) poles))
        agents (loop [i 0, agents agents]
                 (if (< i iter)
                   (let [agents (mapv #(update-agent % poles bounds) agents)]
                     (export-frame i width agents col-neg col-pos)
                     (println "remaining:" (count (filter #(.-active ^Agent %) agents)))
                     (recur (inc i) agents))
                   agents))]
    (println "done")
    agents))

(comment

  (def agents
    (time (field-lines
           {:width   600
            :poles   (repeatedly 50 #(make-dipole 600 max-charge))
            :col-neg (col/rgba 0 1 1)
            :col-pos (col/rgba 1 1 0)
            :numa    20
            :speed   5
            :iter    100})))

  )
