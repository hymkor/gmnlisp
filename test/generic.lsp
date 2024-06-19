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
(test (generic-fun "x" 3) "(2) s=x d=3")
