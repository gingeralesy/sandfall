(in-package #:neato)
(in-readtable :qtools)

(defvar *main* NIL)

(defclass updatable ()
  ())

(defgeneric update (updatable))

(defclass paintable (updatable)
  ())

(defgeneric paint (paintable target))

(define-widget main (QWidget)
  ((objects :initform NIL :accessor objects)))

(define-subwidget (main updater) (q+:make-qtimer main))
(define-subwidget (main background) (q+:make-qcolor 0 0 0))

(define-initializer (main setup)
  (setf *main* main
        (q+:window-title main) "Neato Experiments"
        (q+:single-shot updater) T)
  (push (make-instance 'neato) (objects main))
  (q+:start updater (round (/ 1000 30))))

(define-finalizer (main teardown)
  (v:info :neato.main "EXIT")
  (setf *main* NIL))

(define-slot (main tick) ()
  (declare (connected updater (timeout)))
  (let ((start (internal-time-millis)))
    (loop for obj in (objects main) do
          (update obj))
    (q+:repaint main)
    (q+:start updater (floor (max 0 (- (/ 1000 30)
                                       (- start (internal-time-millis))))))))

(define-override (main paint-event) (ev)
  (with-simple-restart (abort "Abort drawing and continue.")
    (with-finalizing ((painter (q+:make-qpainter main))
                      (bgbrush (q+:make-qbrush background)))
      (setf (q+:style (q+:background painter)) (q+:qt.solid-pattern)
            (q+:color (q+:background painter)) (q+:qt.black)
            (q+:style (q+:brush painter)) (q+:qt.solid-pattern))
      (q+:fill-rect painter (q+:rect main) bgbrush)
      (loop for obj in (objects main) do
            (paint obj painter)))))

(defun internal-time-millis ()
  (/ (get-internal-real-time)
     (/ internal-time-units-per-second
        1000)))

(defun main (&rest initargs)
  (v:output-here)
  (v:info :neato.main "START")
  #+linux (q+:qcoreapplication-set-attribute (q+:qt.aa_x11-init-threads))
  (unwind-protect
       (with-main-window (window (apply #'make-instance 'main initargs)
                          #-darwin :main-thread #-darwin NIL))))