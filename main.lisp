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
  ((objects :initform NIL :accessor objects)
   (input :initform (make-instance 'input-handler) :accessor input)))

(define-subwidget (main updater) (q+:make-qtimer main))

(define-initializer (main setup)
  (setf *main* main
        (q+:window-title main) "Neato Experiments"
        (q+:single-shot updater) T)
  (push (make-instance 'neato) (objects main))
  (q+:start updater (round (/ 1000 30))))

(define-finalizer (main teardown)
  (v:info :neato.main "EXIT")
  (for:for ((obj in (objects main)))
    (finalize obj))
  (setf *main* NIL))

(define-slot (main tick) ()
  (declare (connected updater (timeout)))
  (let ((start (internal-time-millis)))
    (with-simple-restart (abort "Abort update and move to drawing.")
      (for:for ((obj in (objects main)))
        (update obj)))
    (q+:repaint main)
    (q+:start updater (floor (max 0 (- (/ 1000 30)
                                       (- start (internal-time-millis))))))))

(define-override (main paint-event) (ev)
  (with-simple-restart (abort "Abort drawing and continue.")
    (with-finalizing ((painter (q+:make-qpainter main))
                      (bgbrush (q+:make-qbrush (q+:qt.black))))
      (setf (q+:style bgbrush) (q+:qt.solid-pattern))
      (q+:fill-rect painter (q+:rect main) bgbrush)
      (for:for ((obj in (objects main)))
        (paint obj painter)))))

(define-override (main mouse-press-event) (ev)
  (let ((button (qtools:qtenumcase (q+:button ev)
                  ((q+:qt.left-button) :left)
                  ((q+:qt.mid-button) :mid)
                  ((q+:qt.right-button) :right))))
    (mouse-event (input main) 'press (q+:x ev) (q+:y ev) button)))

(define-override (main mouse-release-event) (ev)
  (let ((button (qtools:qtenumcase (q+:button ev)
                  ((q+:qt.left-button) :left)
                  ((q+:qt.mid-button) :mid)
                  ((q+:qt.right-button) :right))))
    (mouse-event (input main) 'release (q+:x ev) (q+:y ev) button)))

(define-override (main mouse-move-event) (ev)
  (mouse-event (input main) 'move (q+:x ev) (q+:y ev) NIL))

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
