(ns ws-ldn-10.ex06
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.color.core :as col]))

(def default-rules
  {:fwd   [:fwd]
   :left  [:left]
   :right [:right]
   :push  [:push]
   :pop   [:pop]})

(def forward-aliases #{:a :b :c :d})

(def dragon-rules
  {:start [:a]
   :a     [:a :right :b :right]
   :b     [:left :a :left :b]})

(def gasket
  {:start [:fwd :left
           :fwd :left
           :fwd :left
           :fwd :left
           :fwd]
   :fwd   [:fwd :left
           :fwd :right :right :fwd :right
           :fwd :left :fwd :left :fwd]})

(def tree-rules
  {:start [:fwd
           :left
           :push :push :start :pop :right :start :pop
           :right
           :fwd
           :push :right :fwd :start :pop
           :left
           :start]
   :fwd   [:fwd :fwd]})

(def penrose
  {:start [:push :b :pop
           :left :left
           :push :b :pop
           :left :left
           :push :b :pop
           :left :left
           :push :b :pop
           :left :left
           :push :b :pop]
   :a     [:c :left :left :d :right :right :right :right :b
           :push :right :c :right :right :right :right :a :pop
           :left :left]
   :b     [:left :c :right :right :d
           :push :right :right :right :a :right :right :b :pop
           :left]
   :c     [:right :a :left :left :b
           :push :left :left :left :c :left :left :d :pop
           :right]
   :d     [:right :right :c :left :left :left :left :a
           :push :left :d :left :left :left :left :b :pop
           :right :right :b]})

(defn rewrite-symbols
  [rules symbols]
  (mapcat (merge default-rules rules) symbols))

(defn make-agent
  [pos theta length branch-theta]
  {:pos          pos
   :theta        theta
   :branch-theta branch-theta
   :length       length
   :path         []
   :stack        []})

(defn save-agent
  [agent]
  (-> agent
      (update :stack conj (dissoc agent :stack))
      (assoc :path [])))

(defn restore-agent
  [agent]
  {:pre [(pos? (count (:stack agent)))]}
  (let [agent' (peek (:stack agent))]
    (assoc agent'
           :stack (pop (:stack agent))
           :path  (into (:path agent') (:path agent)))))

(defmulti interpret
  (fn [agent sym & _] (if (forward-aliases sym) :fwd sym)))

(defmethod interpret :start
  [agent _]
  agent)

(defmethod interpret :fwd
  [agent _]
  (let [pos  (:pos agent)
        pos' (m/+ pos (g/as-cartesian
                       (v/vec2 (:length agent)
                               (:theta agent))))]
    (-> agent
        (assoc :pos pos')
        (update :path conj [pos pos' (count (:stack agent))]))))

(defmethod interpret :left
  [agent _]
  (update agent :theta - (:branch-theta agent)))

(defmethod interpret :right
  [agent _]
  (update agent :theta + (:branch-theta agent)))

(defmethod interpret :push
  [agent _]
  (save-agent agent))

(defmethod interpret :pop
  [agent _]
  (restore-agent agent))

(defmethod interpret :default
  [agent sym]
  #_(prn "no impl for symbol:" sym)
  agent)

(defn interpret-symbols
  [agent syms]
  (reduce interpret agent syms))

(defn visualize-agent
  [path agent]
  (->> agent
       :path
       (map-indexed
        (fn [i [a b depth]]
          (svg/line a b {:stroke (col/hsva (/ i (count (:path agent))) 1 1)})))
       (svg/svg {:width 1000 :height 1000})
       (svg/serialize)
       (spit path)))

(visualize-agent
 "lsys-dragon.svg"
 (interpret-symbols
  (make-agent (v/vec2 500 750) 0 5 m/HALF_PI)
  (last (take 16 (iterate (partial rewrite-symbols dragon-rules) [:start])))))

(visualize-agent
 "lsys-gasket.svg"
 (interpret-symbols
  (make-agent (v/vec2 500 100) 0 10 (m/radians 72))
  (last (take 6 (iterate (partial rewrite-symbols gasket) [:start])))))

(visualize-agent
 "lsys-tree.svg"
 (interpret-symbols
  (make-agent (v/vec2 200 1000) (m/radians -70) 3 (m/radians 25))
  (last (take 8 (iterate (partial rewrite-symbols tree-rules) [:start])))))

(visualize-agent
 "lsys-penrose.svg"
 (interpret-symbols
  (make-agent (v/vec2 500 500) 0 50 (m/radians 36))
  (last (take 6 (iterate (partial rewrite-symbols penrose) [:start])))))
