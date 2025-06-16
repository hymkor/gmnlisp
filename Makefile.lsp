; go run github.com/hymkor/smake@latest {test or release}

(defun tail (path)
  (let ((buf (create-string-output-stream)))
    (format buf "tail \"~A\"" path)
    (sh (get-output-stream-string buf))))

(defun step-exec (command)
  (if (-d ".jj")
    (sh "jj log")
    (sh "git log"))
  (block b
    (while t
      (format (standard-output) "~&$ ~A~%[Y]es: execute, [N]o: skip, [Q]uit ? " command)
      (finish-output (standard-output))
      (case (read-line (standard-input))
        (("q") (return-from b nil))
        (("n") (return-from b t))
        (("y") (sh command) (return-from b t))))))

(defun version-from-release-note (fname)
  (block b
    (let ((line nil) (version nil))
      (if fname
        (with-open-input-file
          (fd fname)
          (while (setq line (read-line fd nil nil))
            (if (setq version (match "^v([0-9]+)\.([0-9]+)\.([0-9]+)$" line))
              (return-from b (car version)))))))
    "v0.0.0"))

(defun find-release-note ()
  (let ((note (wildcard "release_note*.md")))
    (and note (car note))))

(case (and *args* (car *args*))
  (("test")
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
             (tp-all 'verbose))))
       (format (standard-output) "**** Latest log ****~%")
       (tail err-path)
       ); pushd
     ); let
   ); "test"

  (("release")
   (let
     ((j (-d ".jj"))
      (version
        (let ((note (find-release-note)))
          (if note
            (progn
              (format (error-output) "Found: ~A~%" note)
              (version-from-release-note note))
            "v0.0.0"))))
     (and
       (step-exec
         (if j
           (string-append "jj commit -m \"bump to " version "\"")
           (string-append "git commit -m \"bump to " version "\" -a")))
       (step-exec (string-append "git tag " version))
       (if j
         (and (step-exec (string-append "jj bookmark set master -r " version))
              (step-exec "jj git push"))
         (step-exec "git push"))
       (step-exec "git push --tag")
       (step-exec "make dist")
       (step-exec "make release")
       (progn (sh "gh browse")
              (step-exec "make manifest"))
       (if j
         (and (step-exec (string-append "jj commit -m \"Update the manifest of the scoop-installer for " version "\""))
              (step-exec "jj bookmark set master -r @-")
              (step-exec "jj git push"))
         (and (progn (sh "git status")
                     (step-exec (string-append "git commit -a -m \"Update the manifest of the scoop-installer for " version "\"")))
              (step-exec "git push"))))
     ) ; let
   ) ; "release"

  (t
    (format (error-output) "Usage: smake TARGET~%")
    (format (error-output) "  smake release~%")
    (format (error-output) "  smake test~%")
    ) ; t
  ) ; case

