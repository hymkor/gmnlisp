(defun tailrec1 (n)
  (format (standard-output) "[1] ~d~%" n)
  (tailrec1 (+ 1 n)))

(defun tailrec2 (n)
  (format (standard-output) "[2] ~d~%" n)
  (if t
    (tailrec2 (+ 1 n))))

(defun tailrec3 (n)
  (format (standard-output) "[3] ~d~%" n)
  (let ((dummy 1))
    (tailrec3 (+ 1 n))))

(defun tailrec4 (n)
  (format (standard-output) "[4] ~d~%" n)
  (let* ((dummy 1))
    (tailrec4 (+ 1 n))))

(defun tailrec5 (n)
  (format (standard-output) "[5] ~d~%" n)
  (cond
    (t (tailrec5 (+ 1 n)))))

(defun usage ()
  (format (standard-output) "Usage: gmnlisp tailrecopt 1|2|3|4~%"))

(if *posix-argv*
  (case (car *posix-argv*)
    (("1") (tailrec1 1))
    (("2") (tailrec2 1))
    (("3") (tailrec3 1))
    (("4") (tailrec4 1))
    (("5") (tailrec5 1))
    (t (usage)))
  (usage))
