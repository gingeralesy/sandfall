(in-package #:neato)
(in-readtable :qtools)

;; TODO: make the world a series of arrays loaded with 64-bit variables
;;       each variable would represent an area, like the updated array
;;       then each type of a particle, water, sand, etc. would have their own array
;;       and these could then be combined with LOGIOR to see if the tile is taken
;;       it'd also take away the need to make the empty tiles part of the iteration
;;       writing an iterator for Shinmera's FOR would help a bunch as well
(defclass neato (paintable)
  ((world :initform (make-array '(1920 1080)
                                :initial-element 0
                                :element-type '(unsigned-byte 8))
          :accessor world)))

(defmethod update ((neato neato))
  (let* ((world (world neato))
         (width (first (array-dimensions world)))
         (height (second (array-dimensions world)))
         (updated (make-array (list (ceiling (/ (* width height) 64)))
                              :initial-element 0)))
    (dotimes (x width)
      (dotimes (y (1- height))
        (let* ((element (aref world x y))
               (y1 (1+ y))
               (below (aref world x y1))
               (below-left (when (< 0 x) (aref world (1- x) y1)))
               (below-right (when (< x (1- width))
                              (aref world (1+ x) y1)))
               (index (floor (/ (* x y) 64)))
               (update-block (aref updated index))
               (updated-p
                 (< 0 (logand update-block (ash 1 (mod (* x y) 64))))))
          (when (and (not updated-p) (< 0 element))
            (let ((x1 (cond ((= 0 below) x)
                            ((and below-left below-right
                                  (< 0 below-left) (< 0 below-right))
                             (if (< 0 (random 2))
                                 (1+ x)
                                 (1- x)))
                            ((and below-left (< 0 below-left)) (1- x))
                            ((and below-right (< 0 below-right)) (1+ x)))))
              (when x1
                (setf (aref world x y) 0
                      (aref world x1 y1) element))
              (let* ((index (floor (/ (* x1 y1) 64)))
                     (update-block (aref updated index)))
                (setf update-block (logior (ash update-block 1) (if x1 1 0))
                      (aref updated index) update-block)))))))))

(defmethod paint ((neato neato) target)
  (let* ((world (world neato))
         (width (first (array-dimensions world)))
         (height (second (array-dimensions world))))
    (dotimes (x width)
      (dotimes (y height)
        (let ((element (aref world x y)))
          (when (< 0 element)
            (case element
              (1 (q+:set-pen target (q+:qt.blue)))
              (2 (q+:set-pen target (q+:qt.yellow)))
              (T (q+:set-pen target (q+:qt.white))))
            (q+:draw-point x y)))))))
