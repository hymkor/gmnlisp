(lambda ()
  (labels
    ((find-release-note
       ()
       (let ((note (wildcard "release_note*.md")))
         (and note (car note))))

     (version-from-release-note
       (fname)
       (let ((version nil))
         (or
           (file-for-each
             fname
             (lambda (line)
               (if (setq version (match "^v([0-9]+)\.([0-9]+)\.([0-9]+)$" line))
                 (car version))))
           "v0.0.0")))

     (step-exec
       (command)
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
     )
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
        (step-exec (string-append *executable-name* " dist"))
        (step-exec (string-append *executable-name* " release"))
        (progn (sh "gh browse")
               (step-exec (string-append *executable-name* " manifest")))
        (if j
          (and (step-exec (string-append "jj commit -m \"Update the manifest of the scoop-installer for " version "\""))
               (step-exec "jj bookmark set master -r @-")
               (step-exec "jj git push"))
          (and (progn (sh "git status")
                      (step-exec (string-append "git commit -a -m \"Update the manifest of the scoop-installer for " version "\"")))
               (step-exec "git push"))))
      ) ; let
    ) ; labels
  )
