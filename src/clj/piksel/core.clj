(ns piksel.core
  (:require
   [clojure.java.io :as io :only [input-stream output-stream]])
  (:import
   [java.awt Color LinearGradientPaint MultipleGradientPaint$CycleMethod]
   [java.awt.geom Point2D$Float]
   [java.awt.image BufferedImage]
   [javax.imageio ImageIO IIOImage ImageTypeSpecifier]
   [javax.imageio.metadata IIOMetadataNode]))

(def ^:dynamic *default-dpi* 300)

(def ^:const INV8BIT (/ 1.0 255.0))

(defmacro clip8bit
  [x] `(unchecked-int (if (< x 0) 0 (if (> x 0xff) 0xff x))))

(defn ^BufferedImage make-image
  [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))

(defn get-pixels
  [^BufferedImage img]
  (let [w (.getWidth img) h (.getHeight img)]
    (.getRGB img 0 0 w h nil 0 w)))

(defn ^BufferedImage set-pixels
  [^BufferedImage img ^ints pixels]
  (let [w (.getWidth img) h (.getHeight img)]
    (.setRGB img 0 0 w h pixels 0 w)
    img))

(defn ^BufferedImage load-image
  [in]
  (with-open [stream (io/input-stream in)]
    (ImageIO/read stream)))

;;;  ported from Java
;;;  http://stackoverflow.com/questions/321736/how-to-set-dpi-information-in-an-image
(defn save-png
  "Saves image with given DPI resolution as PNG to path."
  ([path img] (save-png path *default-dpi* img))
  ([path dpi img]
   (loop[i (ImageIO/getImageWritersByFormatName "PNG")]
     (when (.hasNext i)
       (let [writer (.next i)
             wparam (.getDefaultWriteParam writer)
             tspec (ImageTypeSpecifier/createFromBufferedImageType BufferedImage/TYPE_INT_RGB)
             imeta (.getDefaultImageMetadata writer tspec wparam)]
         (if (and (not (.isReadOnly imeta)) (.isStandardMetadataFormatSupported imeta))
           (let [dpmm (/ dpi 25.4) ; dpi -> dots per mm
                 hnode (IIOMetadataNode. "HorizontalPixelSize")
                 vnode (IIOMetadataNode. "VerticalPixelSize")
                 dim (IIOMetadataNode. "Dimension")
                 root (IIOMetadataNode. "javax_imageio_1.0")
                 success (volatile! true)]
             (try
               (.setAttribute hnode "value" (str dpmm))
               (.setAttribute vnode "value" (str dpmm))
               (.appendChild dim hnode)
               (.appendChild dim vnode)
               (.appendChild root dim)
               (.mergeTree imeta "javax_imageio_1.0" root)
               (println "saving png:" path "@" dpi "dpi")
               (with-open[stream (ImageIO/createImageOutputStream (io/output-stream path))]
                 (.setOutput writer stream)
                 (.write writer imeta (IIOImage. img nil imeta) wparam))
               (catch Exception e
                 (println "error saving image: " (.getMessage e))
                 (vreset! success false))
               (finally
                 (.dispose writer)))
             @success)
           (recur i)))))))

(defn blend-replace
  [sr sg sb sa r g b a]
  [(min ^long r 0xff)
   (min ^long g 0xff)
   (min ^long b 0xff)
   (min ^double a 1.0)])

(defn blend-alpha
  [sr sg sb sa r g b a]
  [(min (+ sr (* (- r sr) a)))
   (min (+ sg (* (- g sg) a)))
   (min (+ sb (* (- b sb) a)))
   (min (+ sa a) 1.0)])

(defn blend-add
  [sr sg sb sa r g b a]
  [(min (+ sr (* r a)) 0xff)
   (min (+ sg (* g a)) 0xff)
   (min (+ sb (* b a)) 0xff)
   (min (+ sa a) 1.0)])

(defn blend-sub
  [sr sg sb sa r g b a]
  [(max (- sr (* r a)) 0.0)
   (max (- sg (* g a)) 0.0)
   (max (- sb (* b a)) 0.0)
   (min (+ sa a) 1.0)])

(def blend-modes
  {:add blend-add
   :alpha blend-alpha
   :sub blend-sub})

(defmacro unpack-argb
  "Unpacks a 32bit ARGB int into a 4-element vector [a r g b].
  RGB result range is 0x00 - 0xff, alpha 0.0 - 1.0"
  [argb]
  `[(* (bit-and (bit-shift-right ~argb 24) 0xff) INV8BIT)
    (bit-and (bit-shift-right ~argb 16) 0xff)
    (bit-and (bit-shift-right ~argb 8) 0xff)
    (bit-and ~argb 0xff)])

(defmacro pack-argb
  "Packs r g b a into a single 32bit int.
  RGB range is 0x00 - 0xff, alpha 0.0 - 1.0. Does not check ranges!"
  ([rgba]
   `(let* [rgba# ~rgba]
      (unchecked-int
       (bit-or
        (bit-or
         (bit-or
          (bit-shift-left (unchecked-int (* (rgba# 3) 0xff)) 24)
          (bit-shift-left (unchecked-int (rgba# 0)) 16))
         (bit-shift-left (unchecked-int (rgba# 1)) 8))
        (unchecked-int (rgba# 2))))))
  ([r g b a]
   `(unchecked-int
     (bit-or
      (bit-or
       (bit-or
        (bit-shift-left (unchecked-int (* ~a 0xff)) 24)
        (bit-shift-left (unchecked-int ~r) 16))
       (bit-shift-left (unchecked-int ~g) 8))
      (unchecked-int ~b)))))

(defn rgba-array
  "Takes normalized r,g,b,a values (0.0 .. 1.0), converts them to a packed ARGB value,
  then creates & fills a new int array of the given size with it."
  [size r g b a]
  (into-array Integer/TYPE
              (repeat size (pack-argb (* 0xff r) (* 0xff g) (* 0xff b) a))))

(defn fill-array
  [arr r g b a]
  (let [col (pack-argb (* 0xff r) (* 0xff g) (* 0xff b) a)]
    (loop [i (dec (alength arr))]
      (when (>= i 0)
        (aset-int arr i col)
        (recur (dec i))))
    arr))

(defn set-pixel
  ([^ints img x y w r g b a]
   (aset-int img (unchecked-int (+ (* (int y) w) (int x)))
             (pack-argb r g b a)))
  ([^ints img idx r g b a]
   (aset-int img idx (pack-argb r g b a))))

(defn blend-pixel
  ([^ints img x y w r g b a blend-fn]
   (let[idx (unchecked-int (+ (* (int y) w) (int x)))
        [sa sr sg sb] (unpack-argb (aget img idx))]
     (aset-int img idx (pack-argb (blend-fn sr sg sb sa r g b a)))))
  ([^ints img idx r g b a blend-fn]
   (let[[sa sr sg sb] (unpack-argb (aget img idx))]
     (aset-int img idx (pack-argb (blend-fn sr sg sb sa r g b a)))))
  ([^ints img ^ints alt idx f blend-fn]
   (let[[sa sr sg sb] (unpack-argb (aget img idx))
        [ta tr tg tb] (unpack-argb (aget alt idx))]
     (aset-int img idx (pack-argb (blend-fn sr sg sb sa tr tg tb (* f ta)))))))

(defn linear-gradient
  "Creates a linear Java AWT gradient instance.
  http://docs.oracle.com/javase/6/docs/api/java/awt/LinearGradientPaint.html"
  [& {:keys [start end fractions colors] :as opts}]
  (LinearGradientPaint.
   (Point2D$Float. (float (start 0)) (float (start 1)))
   (Point2D$Float. (float (end 0)) (float (end 1)))
   (float-array fractions)
   (into-array Color (map (fn[[r g b a]] (Color. (int r) (int g) (int b) (int a))) colors))
   MultipleGradientPaint$CycleMethod/NO_CYCLE))
