(defgeneric report-condition (s))
(defclass <simple-error> (<error>)
  ((format-string
     :initarg  format-string
     :reader simple-error-format-string)
   (format-arguments
     :initarg  format-arguments
     :reader simple-error-format-arguments)))
(defmethod report-condition ((e <simple-error>) (w <object>))
    (apply #'format
           w
           (simple-error-format-string e)
           (simple-error-format-arguments e)))
(let ((all-properties ()))
  (defun property (sym name)
    (let ((sym-props (assoc sym all-properties)))
      (if sym-props
        ; symbol found
        (let* ((properties (cdr sym-props))
               (name-value (assoc name properties)))
          (if name-value 
            (cdr name-value)
            (error "~A of ~A not found" name sym)))
        ; symbol not found
        (error "Symbol ~A not found" name))))

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
      (let ((sym-props (assoc sym all-properties)))
        (if sym-props
          ; symbol found
          (let ((properties (cdr sym-props)))
            (setq properties (set-item name obj properties))
            (setq all-properties (set-item sym properties all-properties)))
          ; symbol not found
          (let ((properties (list (cons name obj))))
            (setq all-properties (set-item sym properties all-properties))))))

    (defun remove-property (sym name)
      (let ((properties (assoc sym all-properties)))
        (if properties
          (setf (cdr properties) (rm-item name (cdr properties)))
          (setq all-properties (rm-item sym all-properties)))))
    )
  )
