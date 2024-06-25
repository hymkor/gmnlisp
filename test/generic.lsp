(defgeneric generic-fun (p1 p2))

(defmethod generic-fun ((p1 <integer>) (p2 <string>))
  (let ((buf (create-string-output-stream)))
    (format buf "(1) d=~D s=~A" p1 p2)
    (get-output-stream-string buf)))

(defmethod generic-fun ((p1 <string>) (p2 <integer>))
  (let ((buf (create-string-output-stream)))
    (format buf "(2) s=~A d=~D" p1 p2)
    (get-output-stream-string buf)))

(test (generic-fun 1 "s") "(1) d=1 s=s")
(test (generic-fun (+ 1 1) (string-append "[" "]")) "(1) d=2 s=[]")
(test (generic-fun "x" 3) "(2) s=x d=3")
(test (generic-fun "x" (+ 1 2)) "(2) s=x d=3")
(test (generic-function-p #'generic-fun) t)
(test (generic-function-p 1) nil)

(defclass <vector1d> ()
  ((X :initform 0.0 :initarg x :accessor vector-x)))
(defclass <vector2d> (<vector1d>)
  ((Y :initform 0.0 :initarg y :accessor vector-y)))

(defmethod generic-fun ((p1 <vector1d>) (p2 <vector1d>))
  (let ((buf (create-string-output-stream)))
    (format buf "(3) x=~a x=~a" (vector-x p1) (vector-x p2))
    (get-output-stream-string buf)))

(test (generic-fun
        (create <vector1d> 'x 4)
        (create <vector1d> 'x 3))
  "(3) x=4 x=3")

(test (generic-fun
      (create <vector1d> 'x 5)
      (create <vector2d> 'x 6 'y 7))
  "(3) x=5 x=6")

(defgeneric generic-rest (p :rest q))
(defmethod generic-rest ((p <integer>) :rest (q <string>))
  (let ((buf (create-string-output-stream)))
    (format buf "A-~D" p)
    (while q
      (format buf "-~A" (car q))
      (setq q (cdr q)))
    (get-output-stream-string buf)))

(defmethod generic-rest ((p <integer>) :rest (q <integer>))
  (let ((buf (create-string-output-stream)))
    (format buf "B-~D" p)
    (while q
      (format buf "-~A" (car q))
      (setq q (cdr q)))
    (get-output-stream-string buf)))

(test (generic-rest 1 "2" "3") "A-1-2-3")
(test (generic-rest 1 2 3) "B-1-2-3")
