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
  (let ((mouse (mouse input)))
    (setf (x mouse) x
          (y mouse) y)
    (case button
      (:left (setf (left mouse) (eql event-type 'press)))
      (:mid (setf (mid mouse) (eql event-type 'press)))
      (:right (setf (right mouse) (eql event-type 'press))))))

(defmethod keyboard-event ((input input-handler) event-type key)
  (when key
    (if (eql event-type 'press)
        (setf (getf (keys input) key) T)
        (remf (keys input) key))))
