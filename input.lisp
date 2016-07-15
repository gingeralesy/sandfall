(in-package #:neato)
(in-readtable :qtools)

(defclass mouse ()
  ((x :initform 0 :accessor x)
   (y :initform 0 :accessor y)
   (left :initform NIL :accessor left)
   (mid :initform NIL :accessor mid)
   (right :initform NIL :accessor right)
   (keys :initform NIL :accessor keys)))

(defclass input-handler ()
  ((mouse :initform (make-instance 'mouse) :accessor mouse)))

(defmethod mouse-event ((input input-handler) event-type x y button)
  (let ((mouse (mouse input))
        (mouse-button (case button
                        (:left (left mouse))
                        (:mid (mid mouse))
                        (:right (right mouse)))))
    (setf (x mouse) x
          (y mouse) y)
    (when mouse-button
      (setf mouse-button (eql event-type 'press)))))

(defmethod keyboard-event ((input input-handler) event-type key)
  (when key
    (if (eql event-type 'press)
        (setf (getf (keys input) key) T)
        (remf (keys input) key))))
