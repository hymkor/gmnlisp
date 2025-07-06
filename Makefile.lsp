; go run github.com/hymkor/smake@latest {test or release}

(defglobal make (load "smake-go120.lsp"))

(defun tail (path)
  (let ((buf (create-string-output-stream)))
    (format buf "tail \"~A\"" path)
    (sh (get-output-stream-string buf))))

(case (and *args* (car *args*))
  (("verify")
   (if (probe-file "errr")
     (rm "errr"))
   (if (probe-file "err")
     (progn
       (format (standard-output) "**** Previous log ****~%")
       (tail "err")
       (mv "err" "errr")))
   (let* ((curdir (getwd))
          (buf (create-string-output-stream))
          (err-path (join-path curdir "err")))
     (pushd
        "__verify/tp-ipa"

       (load "../../verify.lsp")
       (load-tp-lsp (getenv "TEMP"))
       (with-open-output-file
         (err err-path)
         (with-standard-output
           err
           (with-error-output
             err
             ;(tp-all 'verbose)
             (tp-all)
             )))
       ); pushd
       (format (standard-output) "**** Latest log ****~%")
       (tail err-path)
     ); let
   ); "verify"

  (("test")
    (spawn "./gmnlisp" "test.lsp")
    (funcall make 'fmt)
    (funcall make 'test)
  )

  (("bump")
   (let ((bump (load "smake-bump.lsp")))
     (funcall bump)))

  (("release" "manifest" "clean" "env")
   (funcall make $1))

  (("build" "" nil)
   (funcall make 'build)
   (funcall make 'build-cmd "cmd/gmnlisp"))

  (("dist")
   (dolist (platform (list (cons "linux" "386")
                           (cons "linux" "amd64")
                           (cons "windows" "386")
                           (cons "windows" "amd64")))
     (env (("GOOS"   (car platform))
           ("GOARCH" (cdr platform)))
          (funcall make 'build)
          (let ((aout (funcall make 'build-cmd "cmd/gmnlisp")))
            (funcall make 'dist aout))
          ) ; env
     ) ; dolist
   ) ; "dist"

  (t
    (format (error-output) "Usage: smake {build|dist|release|manifest|bump|test}~%")
    ) ; t
  ) ; case

