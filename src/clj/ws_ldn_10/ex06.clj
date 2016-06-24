(ns ws-ldn-10.ex06
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.svg.core :as svg]))

(defn make-branch
  [& body]
  (conj (vec (cons :branch-start body)) :branch-end))

(def vocab
  {:start        [:forward
                  :left
                  :branch-start
                  :branch-start
                  :start
                  :branch-end
                  :right
                  :start
                  :branch-end
                  :right
                  :forward
                  :branch-start
                  :right
                  :forward
                  :start
                  :branch-end
                  :left
                  :start]
   :forward      [:forward :forward]
   :left         [:left]
   :right        [:right]
   :branch-start [:branch-start]
   :branch-end   [:branch-end]})

(defn rewrite-symbols
  [vocab symbols]
  (mapcat vocab symbols))

;; (rewrite-symbols vocab (rewrite-symbols vocab [:start]))

(defn make-agent
  [pos theta length]
  {:pos    pos
   :theta  theta
   :length length
   :path   []
   :stack  []})

(defn save-agent
  [agent]
  (update agent :stack conj (dissoc agent :stack)))

(defn restore-agent
  [agent]
  {:pre [(pos? (count (:stack agent)))]}
  (let [agent' (peek (:stack agent))]
    (assoc agent'
           :stack (pop (:stack agent))
           :path  (into (:path agent') (:path agent)))))

(defmulti interpret
  (fn [agent sym & _] sym))

(defmethod interpret :start
  [agent _]
  (save-agent agent))

(defmethod interpret :forward
  [agent _]
  (let [pos  (:pos agent)
        pos' (m/+ pos (g/as-cartesian
                       (v/vec2 (:length agent)
                               (:theta agent))))]
    (-> agent
        (assoc :pos pos')
        (update :path conj [pos pos']))))

(defmethod interpret :left
  [agent _]
  (update agent :theta - (m/radians 30)))

(defmethod interpret :right
  [agent _]
  (update agent :theta + (m/radians 30)))

(defmethod interpret :branch-start
  [agent _]
  (save-agent agent))

(defmethod interpret :branch-end
  [agent _]
  (restore-agent agent))

(defn interpret-symbols
  [agent syms]
  (reduce interpret agent syms))

(defn visualize-agent
  [path agent]
  (->> agent
       :path
       (map (fn [[a b]] (svg/line a b {:stroke "black"})))
       (svg/svg {:width 600 :height 600})
       (svg/serialize)
       (spit path)))

#_(visualize-agent
   "lsys.svg"
   (interpret-symbols
    (make-agent (v/vec2 300 100) 0 100)
    [:branch-start :forward :forward :branch-end :left :forward]))

(visualize-agent
 "lsys.svg"
 (interpret-symbols
  (make-agent (v/vec2 300 100) 0 100)
  (last (take 3 (iterate (partial rewrite-symbols vocab) [:start])))))
