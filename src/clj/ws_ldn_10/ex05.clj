(ns ws-ldn-10.ex05
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.sphere :as s]
   [thi.ng.geom.gmesh :as gm]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.ptf :as ptf]
   [thi.ng.geom.mesh.io :as mio]
   [thi.ng.geom.mesh.csg :as csg]
   [thi.ng.geom.mesh.subdivision :as sd]
   [clojure.java.io :as io]))

(def box (a/aabb (v/vec3 1000 -200 300) 100))

(g/center box)

(defn subdivide
  [mesh n subdiv-fn]
  (->> mesh
       (iterate subdiv-fn)
       (drop 1)
       (take n)
       last))

(def box-mesh
  (-> box
      (g/translate [10 20 30])
      (g/rotate-y m/QUARTER_PI)
      (g/scale 2)
      (g/as-mesh {:mesh (gm/gmesh)})
      ;;(g/compute-face-normals)
      (g/tessellate)
      (subdivide 1 sd/catmull-clark)))

(with-open [out (io/output-stream "box.ply")]
  (mio/write-ply (mio/wrapped-output-stream out) box-mesh))

(def a (-> (a/aabb 1)
           (g/as-mesh)
           (csg/mesh->csg)))

(def b (-> (s/sphere (v/vec3 1) 1)
           (g/as-mesh {:res 40})
           (csg/mesh->csg)))

(def result (csg/csg->mesh (csg/union a b)))

(with-open [out (io/output-stream "csg.obj")]
  (mio/write-obj (mio/wrapped-output-stream out) result))

(defn cinquefoil
  [t]
  (let [t  (* t m/TWO_PI)
        pt (* 2.0 t)
        qt (* 7.0 t)
        qc (+ 3.0 (Math/cos qt))]
    (v/vec3
     (* qc (Math/cos pt))
     (* qc (Math/sin pt))
     (Math/sin qt))))

(def ptf-mesh
  (ptf/sweep-mesh
   (mapv cinquefoil (m/norm-range 400))
   (g/vertices (c/circle 0.5) 3)
   {:align? true :loop? true}))

(with-open [out (io/output-stream "ptf.obj")]
  (mio/write-obj (mio/wrapped-output-stream out) ptf-mesh))
