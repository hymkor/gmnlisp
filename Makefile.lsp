; go run github.com/hymkor/smake@latest {test or release}

(defglobal make (load "smake-go120.lsp"))

(defun verify (:rest args)
   (with-error-output
     (standard-output)
     (let ((start-time (get-internal-run-time)))
       (pushd
         "__verify/tp-ipa"
         (apply #'spawn "../../gmnlisp"
                "-strict"
                "../../verify.lsp"
                (getenv "TEMP")
                args))
       (format (error-output)
               "~%Elapsed time: ~A seconds~%"
               (quotient (- (get-internal-run-time) start-time)
                         (internal-time-units-per-second)))))
   )

(case (and *args* (car *args*))
  (("verify")
   (verify))

  (("verify-verbose")
   (verify "verbose"))

  (("test")
   (spawn "./gmnlisp" "test.lsp")
   (funcall make 'fmt)
   (funcall make 'test))

  (("bump")
   (let ((bump (load "smake-bump.lsp")))
     (funcall bump)))

  (("release" "manifest" "clean" "env")
   (funcall make $1))

  (("build" "" nil)
   (funcall make 'build)
   (funcall make 'build-cmd "cmd/gmnlisp"))

  (("dist")
   (mapc
     (lambda (platform)
       (env (("GOOS"   (car platform))
             ("GOARCH" (cdr platform)))
            (funcall make 'build)
            (let ((aout (funcall make 'build-cmd "cmd/gmnlisp")))
              (funcall make 'dist aout))
            ))
     (list (cons "linux" "386")
           (cons "linux" "amd64")
           (cons "windows" "386")
           (cons "windows" "amd64")))
   )

  (t
    (format (error-output) "Usage: smake {build|dist|release|manifest|bump|test}~%")
    ) ; t
  ) ; case

