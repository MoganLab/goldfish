(import (liii check) (liii os) (liii subprocess) (liii path))

(when (os-linux?)
  ;; run basic tests
  (check (zero? (run "true")) => #t)
  (check (zero? (run "false")) => #f)
  (check (run "echo hello") => 0)
  (check (zero? (run '(true))) => #t)
  (check (zero? (run '(false))) => #f)

  (let ((orig-dir (getcwd)))
    (run "pwd" :cwd "/tmp")
    (check (getcwd) => orig-dir)
  ) ;let

  (let ((orig-dir (getcwd)))
    (run '(pwd) :cwd "/tmp")
    (check (getcwd) => orig-dir)
  ) ;let

  (check-catch 'value-error (run "cd /tmp" :cwd "/home"))
  (check-catch 'value-error (run '(cd "/tmp") :cwd "/home"))

  ;; run-set! / run-get tests
  (run-set! 'echo-cmd "/bin/echo")
  (check (zero? (run '(echo-cmd "hello"))) => #t)

  (run-set! 'true-cmd "/bin/true")
  (check (zero? (run '(true-cmd))) => #t)

  (run-set! 'my-lambda (lambda () (display "ok\n")))
  (check (zero? (run '(my-lambda))) => #t)

  (check (path? (run-get 'echo-cmd)) => #t)
  (check (run-get 'not-exist) => #f)

  (run-set! 'custom-lambda (lambda () (display "ok\n")))
  (check (procedure? (run-get 'custom-lambda)) => #t)

  ;; run-allow! tests
  (run-allow! 'echo-cmd)
  (check (zero? (run '(echo-cmd "hello"))) => #t)
  (check-catch 'value-error (run '(true-cmd)))

  (run-allow! '())
  (check (zero? (run '(true-cmd))) => #t)

  (run-allow! '(echo-cmd true-cmd))
  (check (zero? (run '(echo-cmd "hello"))) => #t)
  (check (zero? (run '(true-cmd))) => #t)
  (check-catch 'value-error (run '(unknown-cmd)))

  (run-allow! '())

  ;; run-ban! tests
  (run-ban! 'echo-cmd)
  (check-catch 'value-error (run '(echo-cmd "hello")))
) ;when

(check-report)
