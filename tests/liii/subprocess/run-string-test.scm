(import (liii check) (liii os) (liii path) (liii subprocess) (scheme file))

(when (os-linux?)
  (check (run-string "echo hello") => "hello\n")
  (check (run-string '("echo" "hello")) => "hello\n")

  (check-catch 'value-error (run-string "false"))

  (check (run-string "pwd" :cwd "/tmp") => "/tmp\n")

  ;; :env
  (check (run-string "echo $FOO" :env '(("FOO" . "bar"))) => "bar\n")

  ;; :input
  (check (run-string "cat" :input "hello world") => "hello world")

  ;; :stdout to file
  (let ((tmpfile "/tmp/gf-run-string-stdout-test.txt"))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (check (run-string "echo hello" :stdout tmpfile) => "")
    (check (path-read-text tmpfile) => "hello\n")
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :stdout 'discard
  (check (run-string "echo hello" :stdout 'discard) => "")

  ;; :stderr 'stdout
  (check (run-string "echo hello >&2" :stderr 'stdout) => "hello\n")

  ;; :stderr 'discard
  (check (run-string "echo hello >&2" :stderr 'discard) => "")
) ;when

(check-report)
