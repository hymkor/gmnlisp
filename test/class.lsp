(defclass <point1d> ()
  ((X :initform 0.0 :initarg x :accessor point-x :reader get-point-x :writer let-point-x)))

(defclass <point2d> (<point1d>)
  ((Y :initform 0.0 :initarg y :accessor point-y :reader get-point-y :writer let-point-y)))

(defclass <point3d> (<point2d>)
  ((Z :initform 0.0 :initarg z :accessor point-z :reader get-point-z :writer let-point-z)))

(let
  ((p1 (create <point2d> 'x 3))
   (p2 (create <point3d> 'x 10 'y 20 'z 30)))

  ; test accessor as getter
  (test (point-x p1) 3)
  (test (point-y p1) 0.0)

  ; test reader
  (test (get-point-x p1) 3)
  (test (get-point-y p1) 0.0)

  ; test accessor as setter
  (setf (point-x p1) 88)
  (test (point-x p1) 88)

  ; test writer
  (let-point-y 99 p1)
  (test (point-y p1) 99)

  ; test super class
  (test (point-x p2) 10)
  (test (point-y p2) 20)
  (test (point-z p2) 30)

  (test (create <integer>) 0)
  (test (create <float>) 0.0)
  (test (create <string>) "")
  )

(defclass <foo> ()
  ((X :initarg x :accessor x-of-foo :boundp has-x)))
(let ((f1 (create <foo> 'x 0))
      (f2 (create <foo>)))
  (test (has-x f1) t)
  (test (has-x f2) nil)
  (setf (x-of-foo f2) 1)
  (test (has-x f2) t))
