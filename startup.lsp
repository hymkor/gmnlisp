(defmethod report-condition ((e <simple-error>) (w <object>))
    (apply #'format
           w
           (simple-error-format-string e)
           (simple-error-format-arguments e)))
(let ((all-properties ()))
  (defun property (sym name &rest objs)
    (if (> (length objs) 2)
      (error "too many arguments"))
    (assure <symbol> sym)
    (assure <symbol> name)
    (let ((sym-props (assoc sym all-properties)))
      (if sym-props
        ; symbol found
        (let* ((properties (cdr sym-props))
               (name-value (assoc name properties)))
          (if name-value
            ; name found
            (cdr name-value)
            ; name not found
            (and objs (car objs))))
        ; symbol not found
        (and objs (car objs)))))

  (labels
    ((rm-item
       (key L)
       (if L
         (if (equal key (car (car L)))
           (rm-item key (cdr L))
           (cons (car L) (rm-item key (cdr L))))
         nil))
     (set-item
       (key val L)
       (cons (cons key val) (rm-item key L))))

    (defun set-property (obj sym name)
      (assure <symbol> sym)
      (assure <symbol> name)
      (let ((sym-props (assoc sym all-properties)))
        (if sym-props
          ; symbol found
          (let ((properties (cdr sym-props)))
            (setq properties (set-item name obj properties))
            (setq all-properties (set-item sym properties all-properties)))
          ; symbol not found
          (let ((properties (list (cons name obj))))
            (setq all-properties (set-item sym properties all-properties)))))
      obj)

    (defun remove-property (sym name)
      (assure <symbol> sym)
      (assure <symbol> name)
      (let ((result (property sym name))
            (properties (assoc sym all-properties)))
        (if properties
          (setf (cdr properties) (rm-item name (cdr properties)))
          (setq all-properties (rm-item sym all-properties)))
        result))
    )
  )
