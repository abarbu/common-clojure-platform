(ns common-clojure-platform.core)

(use '[monads core cont util])
(use 'sanity.core)
(use 'sanity.improvements)
(use 'sanity.reader)
(use 'clojure.pprint)
(require '[clatrix.core :as c])
(require '[clojure.string :as s])
(require '[clojure.java.io :as jio])
(require '[taoensso.timbre :as log])
(require '[clansi.core :as color])
(use 'common-clojure.core)

;; Load up OpenCV
(clojure.lang.RT/loadLibrary org.opencv.core.Core/NATIVE_LIBRARY_NAME)

(import java.io.StringWriter java.io.File)
(import 'java.text.NumberFormat)

(combine-namespaces
 i
 incanter.bayes incanter.censored incanter.charts incanter.core incanter.datasets
 incanter.distributions incanter.excel incanter.infix incanter.interpolation
 incanter.io incanter.latex incanter.mongodb incanter.optimize incanter.pdf
 incanter.som
 incanter.stats incanter.svg incanter.symbolic incanter.zoo)

;;; OpenCV

(import '[org.opencv.core Mat Size CvType MatOfByte Core Point Scalar]
        '[org.opencv.highgui Highgui VideoCapture]
        '[org.opencv.imgproc Imgproc])

;; Raw video API

(define (open-video% ^String filename)
 (let [v (VideoCapture.)]
  (.open v filename)
  (when (not (.isOpened v)) (throw (ex-info "Can't open" {:filename filename})))
  (.grab v)
  v))

(define (close-video% ^VideoCapture video) (.release video))

(define (with-video% filename f)
 (let [v (open-video% filename)
       r (f v)]
  (close-video% v)
  r))

(define (next-frame% ^VideoCapture video) (.grab video))

(define (compute-video-length% video-path)
 (with-video%
  video-path
  (fn [video]
   (loop [i 0] (if (next-frame% video) (recur (+ i 1)) i)))))

(define (video-length video-path)
 (if (:length video-path)
  video-path
  (let [length-file (replace-extension video-path "video-length")]
   (or (if (file-exists? length-file)
        (let [data (read-object-from-file length-file)
              mod-time (file-change-time video-path)]
         (if (and (seq? data) (= (length data) 2) (every? number? data)
                  (= (second data) mod-time))
          (first data)
          false))
        false)
       ;; * matters because we want the time computed before the
       ;; video length to avoid race conditions
       (let ((mod-time (file-change-time video-path))
             (l (compute-video-length% video-path)))
        (write-object-to-file (list l mod-time) length-file)
        l)))))

;; High-level video API

(defrecord opencv-video [^VideoCapture handle ^String file ^int length width height])

(define (open-video ^String filename)
 (let [video (open-video% filename)
       ^Mat m (Mat.)]
  (.retrieve ^VideoCapture video m)
  (->opencv-video video filename (video-length filename)
                  (.width m)
                  (.height m))))

(define (close-video ^opencv-video video) (close-video% (:handle video)))

(define (with-video filename f)
 (let [v (open-video filename)
       r (f v)]
  ;; FIXME This is a hack because the JVM does not gc and run
  ;; finalizers often enough to clean up OpenCV's garbage in a timely
  ;; fashion. Mat objects are tiny on the JVM's heap but large on the
  ;; system heap. This means that only a few kb JVM heap space can
  ;; correspond to enogh memory to exhaust all RAM.
  (System/gc)
  (close-video v)
  r))

(define (next-frame! ^opencv-video video) (next-frame% (:handle video)))
(define (read-frame ^opencv-video video) (let [m (Mat.)] (if (.read ^VideoCapture (:handle video) m) m nil)))
(define (peek-frame ^opencv-video video) (let [m (Mat.)] (if (.retrieve ^VideoCapture (:handle video) m) m nil)))

;;; OpenCV Image

(defn draw-rectangle [image box colour thickness]
 (Core/rectangle image
                 (Point. (nth box 0) (nth box 1))
                 (Point. (nth box 2) (nth box 3))
                 (Scalar. (nth colour 2) (nth colour 1) (nth colour 0))
                 thickness))

(defn image-load [s] (Highgui/imread s))
(defn image-save [s i] (Highgui/imwrite s i))

(defrecord image-buffer [^"[B" buffer ^long width ^long height ^long channels ^clojure.lang.Symbol format])
(. clojure.pprint/simple-dispatch addMethod image-buffer (fn [& more] (print more)))

(defn image->image-buffer [^org.opencv.core.Mat i]
 (let ((buffer (byte-array (* (.width i) (.height i) (.channels i)))))
  (.get i 0 0 buffer)
  (->image-buffer buffer (.width i) (.height i) (.channels i) 'bgr)))

;; Show

(import '(javax.swing JFrame JLabel JTextField JButton ImageIcon)
        '(javax.imageio ImageIO)
        '(java.io ByteArrayInputStream)
        '(java.awt.event ActionListener)
        '(java.awt GridLayout))

(define (imshow ^Mat img)
 (let [m (MatOfByte.)]
  (Highgui/imencode ".jpg" img m)
  (let [frame (JFrame.)]
   (.add
    (.getContentPane frame)
    (JLabel. (ImageIcon. (ImageIO/read (ByteArrayInputStream. (.toArray m))))))
   (.pack frame)
   (.setVisible frame true))))

;; Incanter integration
(defmethod i/view org.opencv.core.Mat ([obj & options] (imshow obj)))
(define view i/view)

(define (video-first-frame video-path) 1)
(define (video-last-frame video-path) (video-length video-path))

(define (for-each-frame f v)
 (for-each-m-n f (video-first-frame v) (video-last-frame v)))

(define (for-each-frame-reversed f v)
 (for-each-m-n-dec f (video-last-frame v) (video-first-frame v)))

(define (map-frame f v)
 (map-m-n f (video-first-frame v) (video-last-frame v)))

(define (map-frame-indexed f v)
 (let ((first-frame (video-first-frame v)))
  (map-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (video-last-frame v))))

(define (for-each-frame-indexed f v)
 (let ((first-frame (video-first-frame v)))
  (for-each-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (video-last-frame v))))

(define (map-frame-indexed-but-last f v)
 (let ((first-frame (video-first-frame v)))
  (map-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (- (video-last-frame v) 1))))

(define (for-each-frame-indexed-but-last f v)
 (let ((first-frame (video-first-frame v)))
  (for-each-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (- (video-last-frame v) 1))))

(define (for-each-frame-but-last f v)
 (for-each-m-n f (video-first-frame v) (- (video-last-frame v) 1)))

(define (map-frame-but-last f v)
 (map-m-n f (video-first-frame v) (- (video-last-frame v) 1)))

(define (map-frame-pair individual-f pair-f video-path)
 (let ((first-frame (video-first-frame video-path))
       (last-frame (video-last-frame video-path)))
  (loop ((n (+ first-frame 1))
         (prev (individual-f first-frame))
         (result '()))
   (if (> n last-frame)
    (reverse result)
    (let ((next (individual-f n)))
     (recur (+ n 1) next (cons (pair-f prev next) result)))))))

(define (for-each-frame-pair individual-f pair-f video-path)
 (let ((first-frame (video-first-frame video-path))
       (last-frame (video-last-frame video-path)))
  (loop ((n (+ first-frame 1))
         (prev (individual-f first-frame)))
   (unless (> n last-frame)
    (let ((next (individual-f n)))
     (pair-f prev next)
     (recur (+ n 1) next))))))

(define (map-image-from-video-indexed f video-path)
 (with-video
  video-path
  (lambda (video)
   (map-frame-indexed
    ;; Frame-nr is the current frame number -- the first frame may not be zero
    ;; Index is the frame offset (starting at 0) in the video file
    (lambda (frame-nr index)
     (let ((result (f frame-nr index (peek-frame video))))
      (next-frame! video)
      result))
    video-path))))

(define (for-each-image-from-video-indexed f video-path)
 (with-video
  video-path
  (lambda (video)
   (for-each-frame-indexed
    ;; Frame-nr is the current frame number -- the first frame may not be zero
    ;; Index is the frame offset (starting at 0) in the video file
    (lambda (frame-nr index)
     (f frame-nr index (peek-frame video))
     (next-frame! video))
    video-path))))

(define (for-each-image-from-video-indexed-but-last f video-path)
 (with-video
  video-path
  (lambda (video)
   (for-each-frame-indexed-but-last
    ;; Frame-nr is the current frame number -- the first frame may not be zero
    ;; Index is the frame offset (starting at 0) in the video file
    (lambda (frame-nr index)
     (f frame-nr index (peek-frame video))
     (next-frame! video))
    video-path))))

(define (map-image-pair-from-video individual-f pair-f video-path)
 ;; individual-f :: frame-nr -> imlib -> a
 ;; pair-f :: a -> a -> b
 (with-video
  video-path
  (lambda (video)
   (map-frame-pair
    (lambda (frame-nr)
     (let ((frame-data (peek-frame video)))
      (next-frame! video)
      (individual-f frame-nr frame-data)))
    pair-f
    video-path))))

(define (for-each-image-pair-from-video-indexed individual-f pair-f video-path)
 ;; individual-f :: frame-nr -> index -> imlib -> a
 ;; pair-f :: a -> a -> b
 (letfn [(for-each-frame-pair-indexed [individual-f pair-f video-path]
           (let ((first-frame (video-first-frame video-path))
                 (last-frame (video-last-frame video-path)))
            (loop ((n (+ first-frame 1))
                   (i 1)
                   (prev (individual-f first-frame 0)))
             (unless (> n last-frame)
              (let ((next (individual-f n i)))
               (pair-f prev next)
               (recur (+ n 1) (+ i 1) next))))))]
  (with-video
   video-path
   (lambda (video)
    (for-each-frame-pair-indexed
     (lambda (frame-nr index)
      (let ((frame-data (peek-frame video)))
       (next-frame! video)
       (individual-f frame-nr index frame-data)))
     pair-f
     video-path)))))

(define (for-each-image-pair-from-video individual-f pair-f video)
 (for-each-image-pair-from-video-indexed
  (lambda (frame-nr index frame-data)
   (individual-f frame-nr frame-data))
  pair-f
  video))
