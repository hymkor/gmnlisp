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

  (test (instancep p1 <point2d>) t)
  (test (instancep p2 <point2d>) t)
  (test (instancep p1 <point3d>) nil)
  (test (instancep 0 <point2d>) nil)
  (test (assure <point2d> p1) p1)
  (test (the <point2d> p1) p1)
  (test
    (catch
      'c
      (with-handler
        (lambda (c)
          (if (instancep c <domain-error>)
            (throw 'c "OK")
            "NG1"))
        (assure <point3d> p1)
        "NG2"
        )
      )
    "OK")
  )

(test (subclassp <point1d> <point2d>) nil)
(test (subclassp <point1d> <point1d>) nil)
(test (subclassp <point2d> <point1d>) t)

(defclass <foo> ()
  ((X :initarg x :accessor x-of-foo :boundp has-x)))
(let ((f1 (create <foo> 'x 0))
      (f2 (create <foo>)))
  (test (has-x f1) t)
  (test (has-x f2) nil)
  (setf (x-of-foo f2) 1)
  (test (has-x f2) t))

(defmethod initialize-object ((this <foo>) (x <string>))
  (setf (x-of-foo this) x)
  this)
(let ((f1 (create <foo> (string-append "x" "y"))))
  ; (format (standard-output) "~S~%" f1)
  (test (x-of-foo f1) "xy"))

(test (subclassp <error> <object>) t)
(test (subclassp <object> <error>) nil)
(test (subclassp <error> <error>) nil)
