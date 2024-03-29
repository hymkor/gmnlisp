(defmacro when (test &rest args)
  `(if ,test (progn ,@args)))

(defmacro unless (test &rest args)
  `(if ,test nil (progn ,@args)))

(defmacro prog1 (expr &rest args)
  (let ((x (gensym)))
    `(let ((,x ,expr))
       (progn ,@args)
       ,x)))

(defmacro prog2 (expr1 expr2 &rest args)
  `(progn ,expr1 (prog1 ,expr2 ,@args)))

(defun 1+ (x) (+ x 1))

(defmacro incf (place &rest args)
  (let ((delta (if args (car args) 1)))
    `(setf ,place (+ ,place ,delta))))

(defun 1- (x) (- x 1))

(defmacro decf (place &rest args)
  (let ((delta (if args (car args) 1)))
    `(setf ,place (- ,place ,delta))))

(defun swap-elt (newvalue source z)
  (if (stringp source)
    (string-append
      (subseq source 0 z)
      (create-string 1 newvalue)
      (subseq source (1+ z) (length source)))
    (let ((s source))
      (while s
        (if (zerop z)
          (set-car newvalue s))
        (decf z)
        (setq s (cdr s))
        )
      source)))

(defun swap-subseq (seq start end newvalue)
  (if (stringp seq)
    (string-append (subseq seq 0 start)
                   newvalue
                   (subseq seq end (length seq)))
    (let ((orig seq))
      (while seq
        (if (and (<= start 0) (> end 0) newvalue)
          (progn
            (set-car (car newvalue) seq)
            (setq newvalue (cdr newvalue))))
        (decf start)
        (decf end)
        (setq seq (cdr seq)))
      orig)))

(let ((setf-table
        '((car . set-car)
          (cdr . set-cdr)
          (elt . set-elt)
          (dynamic . set-dynamic)
          (subseq . set-subseq)
          (setq . set-setq)
          (assoc . set-assoc)
          (aref . set-aref)
          (cddr . set-cddr)
          (cdddr . set-cdddr)
          (cadr . set-cadr)
          (caddr . set-caddr)
          (cadddr . set-cadddr)
          (first . set-car)
          (second . set-cadr)
          (third . set-caddr)
          (gethash . set-gethash))))
  (defmacro setf (expr newvalue)
    (if (symbolp expr)
      `(setq ,expr ,newvalue)
      (let* ((name (car expr))
             (pair (assoc name setf-table))
             (tmp nil)
             (setter
               (if pair
                 (cdr pair)
                 (progn
                   (setq tmp (convert (string-append "set-" (convert name <string>)) <symbol>))
                   (setq setf-table (cons (cons name tmp) setf-table))
                   tmp)))
             (arguments (cdr expr)))
        (cons setter (cons newvalue arguments))))))

(defmacro set-elt (newvalue seq &rest z)
  `(if (arrayp ,seq)
     (set-aref ,newvalue ,seq ,@z)
     (setf ,seq (swap-elt ,newvalue ,seq ,@z))))

(defmacro set-dynamic (newvalue name)
  `(defdynamic ,name ,newvalue))

(defmacro set-subseq (newvalue seq start end)
  `(setf ,seq (swap-subseq ,seq ,start ,end ,newvalue)))

(defmacro set-setq (newvalue name avlue)
  (let ((name (elt expr 1)) (value (elt expr 2)))
    `(progn (setq ,name ,value) (setf ,name ,newvalue))))

(defmacro set-assoc (newvalue key m)
  (let ((L (gensym)) (K (gensym)) (tmp (gensym)))
    `(let* ((,L ,m)
            (,K ,key)
            (,tmp nil))
       (while ,L
         (if (and (setq ,tmp (car ,L)) (consp ,tmp) (equal ,K (car ,tmp)))
           (set-car ,newvalue ,L))
         (setq ,L (cdr ,L))))))

(defmacro dolist (vars &rest body)
  (let ((var (car vars))
        (values (elt vars 1))
        (rest (gensym)))
    `(block
       nil
       (let ((,var nil)(,rest ,values))
         (while ,rest
           (setq ,var (car ,rest))
           (setq ,rest (cdr ,rest))
           ,@body)))))

(defmacro dotimes (vars &rest commands)
  (let ((var (car vars))
        (count (elt vars 1))
        (end (gensym)))
    `(let ((,var 0)(,end ,count))
       (while (< ,var ,end)
         (progn ,@commands)
         (setq ,var (+ 1 ,var))))))

(defmacro for (iters test-result &rest body)
  (let ((inits nil)
        (steps nil)
        (test (car test-result))
        (result (elt test-result 1)))
    (while iters
      (let ((e (car iters)))
        (setq inits (cons (list (car e) (elt e 1)) inits))
        (setq steps (append steps (list (car e) (elt e 2)))))
      (setq iters (cdr iters)))
    (setq steps (cons 'psetq steps))
    `(let ,inits
       (while (not ,test)
         ,@body
         ,steps)
       ,result)))

(defun nthcdr (n x)
  (block
    nil
    (while (and (>= (decf n) 0) x)
      (unless (consp x)
        (return nil))
      (setq x (cdr x)))
    x))

(defun cddr (x) (nthcdr 2 x))

(defun cdddr (x) (nthcdr 3 x))

(defun cadr (x) (elt x 1))

(defun caddr (x) (elt x 2))

(defun cadddr (x) (elt x 3))

(defun first (x) (car x))

(defun second (x) (elt x 1))

(defun third (x) (elt x 2))

(defun rest (x) (cdr x))

(defun nth (n x) (elt x n))

(defun set-nth (newvalue Z L)
  (set-elt newvalue L Z))

(defun set-nthcdr (newvalue z source)
  (let ((s source))
    (while s
           (setq z (1- z))
           (if (zerop z)
             (set-cdr newvalue s))
           (setq s (cdr s)))
    source))

(defun set-cadr (newvalue L)
  (set-car newvalue (cdr L)))

(defun set-caddr (newvalue L)
  (set-car newvalue (cdr (cdr L))))

(defun set-cadddr (newvalue L)
  (set-car newvalue (cdr (cdr (cdr L)))))

(defun set-cddr (newvalue L)
  (set-cdr newvalue (cdr L)))

(defun set-cdddr (newvalue L)
  (set-cdr newvalue (cdr (cdr L))))

(defmacro flet (e &rest body)
  (let (result ee)
    (while e
      (setq ee (car e))
      (setq result (cons (cons (car ee) (list (cons 'lambda (cdr ee)))) result))
      (setq e (cdr e)))
    `(let ,result ,@body)))

(defmacro labels (e &rest body)
  (let (result ee)
    (while e
      (setq ee (car e))
      (setq result (cons (cons (car ee) (list (cons 'lambda (cdr ee)))) result))
      (setq e (cdr e)))
    (setq result (nreverse result))
    `(let* ,result ,@body)))
