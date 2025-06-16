; The ISLisp verification system assumes that /tmp exists and
; uses it for temporary files. On Windows, this script replaces
; the constant *tp-tmp-dir* with the value of the environment
; variable TEMP and then loads the modified verification code.
;
; This script is available for both gmnlisp and smake, and on
; Linux, it behaves the same as a regular (load).

(defun load-tp-lsp (tmpdir)
  (if (or (null tmpdir) (string= tmpdir ""))
    (setq tmpdir nil)
    (let* ((tail (elt tmpdir (- (length tmpdir) 1))))
      (if (and (not (equal tail #\/)) (not (equal tail #\\)))
        (setq tmpdir (string-append tmpdir "/")))))

  (let ((codes (list 'progn)))
    (with-open-input-file
      (fd "tp.lsp")
      (let ((code1 nil))
        (while (setq code1 (read fd nil nil))
          (if (and tmpdir
                   (listp code1)
                   (= (length code1) 3)
                   (equal (elt code1 0) 'defconstant)
                   (equal (elt code1 1) '*tp-tmp-dir*))
            (setq code1 (list 'defconstant '*tp-tmp-dir* tmpdir)))
          (setq codes (cons code1 codes)))
        )
      )
    (setq codes (nreverse codes))
    (eval codes)))

(defun is-gmnlisp-exe ()
  (catch 'fail
    (with-handler
      (lambda (c) (throw 'fail nil))
      *posix-argv*)))

(if (is-gmnlisp-exe)
  (progn
    (load-tp-lsp (and (consp *argv*) (car *argv*)))
    (let ((options nil))
      (if (consp *argv*)
        (setq options
              (mapcar (lambda (s)
                        (let ((reader (create-string-input-stream s)))
                          (read reader nil nil))) (cdr *argv*))))
      (apply #'tp-all options))
    ))
