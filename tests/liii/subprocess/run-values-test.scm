(import (liii check) (liii os) (liii path) (liii subprocess) (scheme file))

(when (os-linux?)
  (let-values (((out err code) (run-values "echo hello")))
    (check out => "hello\n")
    (check err => "")
    (check (zero? code) => #t)
  ) ;let-values

  (let-values (((out err code) (run-values "false")))
    (check out => "")
    (check (zero? code) => #f)
  ) ;let-values

  ;; :env
  (let-values (((out err code) (run-values "echo $FOO" :env '(("FOO" . "bar")))))
    (check out => "bar\n")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :input
  (let-values (((out err code) (run-values "cat" :input "hello world")))
    (check out => "hello world")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :timeout
  (let-values (((out err code) (run-values "sleep 10" :timeout 1)))
    (check code => -1)
  ) ;let-values

  ;; :stdout to file
  (let ((tmpfile "/tmp/gf-run-values-stdout-test.txt"))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (let-values (((out err code) (run-values "echo hello" :stdout tmpfile)))
      (check out => "")
      (check (zero? code) => #t)
      (check (path-read-text tmpfile) => "hello\n")
    ) ;let-values
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :stdout 'discard
  (let-values (((out err code) (run-values "echo hello" :stdout 'discard)))
    (check out => "")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :stderr 'stdout
  (let-values (((out err code) (run-values "echo hello >&2" :stderr 'stdout)))
    (check out => "hello\n")
    (check err => "")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :stderr 'discard
  (let-values (((out err code) (run-values "echo hello >&2" :stderr 'discard)))
    (check out => "")
    (check err => "")
    (check (zero? code) => #t)
  ) ;let-values

  ;; :stdin from file
  (let ((tmpfile "/tmp/gf-run-values-stdin-test.txt"))
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
    (call-with-output-file tmpfile (lambda (p) (display "file content\n" p)))
    (let-values (((out err code) (run-values "cat" :stdin tmpfile)))
      (check out => "file content\n")
      (check (zero? code) => #t)
    ) ;let-values
    (when (file-exists? tmpfile)
      (delete-file tmpfile)
    ) ;when
  ) ;let

  ;; :stdin 'null
  (let-values (((out err code) (run-values "cat" :stdin 'null)))
    (check out => "")
    (check (zero? code) => #t)
  ) ;let-values
) ;when

(check-report)
