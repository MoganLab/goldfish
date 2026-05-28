(import (liii check) (liii os) (liii path) (liii subprocess) (scheme file))

(when (os-linux?)
  ;; Basic success
  (check (run-and "true" "true") => 0)

  ;; First failure stops
  (check (run-and "false" "true") => 1)

  ;; Last command failure
  (check (run-and "true" "false") => 1)

  ;; :cwd shared
  (check (run-and "true" "true" :cwd "/tmp") => 0)

  ;; :env shared
  (check (run-and "test $FOO = bar" :env '(("FOO" . "bar"))) => 0)

  ;; :stdout only on last
  (let ((tmpfile "/tmp/gf-run-and-stdout.txt"))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (check (run-and "echo a" "echo b" :stdout tmpfile) => 0)
    (check (path-read-text tmpfile) => "b\n")
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :input only on first
  (check (run-and "cat" "true" :input "hello") => 0)

  ;; Multiple commands
  (check (run-and "true" "true" "true") => 0)
  (check (run-and "true" "false" "true") => 1)
) ;when

(check-report)
