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

(when (os-windows?)
  ;; run basic tests
  (check (zero? (run "python3 -c pass")) => #t)
  (check (zero? (run "python3 -c 1/0")) => #f)
  (check (run "python3 -c print('hello')") => 0)

  (let ((orig-dir (getcwd)))
    (run "python3 -c pass" :cwd (os-temp-dir))
    (check (getcwd) => orig-dir)
  ) ;let

  (check-catch 'value-error (run "cd /tmp" :cwd (os-temp-dir)))
  (check-catch 'value-error (run '(cd "/tmp") :cwd (os-temp-dir)))

  ;; run-set! / run-get tests
  (run-set! 'pytrue "python3")
  (check (zero? (run '(pytrue "-c" "pass"))) => #t)

  (run-set! 'py-lambda (lambda () (display "ok\n")))
  (check (zero? (run '(py-lambda))) => #t)

  (check (path? (run-get 'pytrue)) => #t)
  (check (run-get 'not-exist) => #f)

  (run-set! 'custom-lambda (lambda () (display "ok\n")))
  (check (procedure? (run-get 'custom-lambda)) => #t)

  ;; run-allow! tests
  (run-allow! 'pytrue)
  (check (zero? (run '(pytrue "-c" "pass"))) => #t)
  (check-catch 'value-error (run '(py-lambda)))

  (run-allow! '())
  (check (zero? (run '(py-lambda))) => #t)

  (run-allow! '(pytrue py-lambda))
  (check (zero? (run '(pytrue "-c" "pass"))) => #t)
  (check (zero? (run '(py-lambda))) => #t)
  (check-catch 'value-error (run '(unknown-cmd)))

  (run-allow! '())

  ;; run-ban! tests
  (run-ban! 'pytrue)
  (check-catch 'value-error (run '(pytrue "-c" "pass")))
) ;when

(when (os-macos?)
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

  (check-catch 'value-error (run "cd /tmp" :cwd "/tmp"))
  (check-catch 'value-error (run '(cd "/tmp") :cwd "/tmp"))

  ;; run-set! / run-get tests
  (run-set! 'echo-cmd "/bin/echo")
  (check (zero? (run '(echo-cmd "hello"))) => #t)

  (run-set! 'my-lambda (lambda () (display "ok\n")))
  (check (zero? (run '(my-lambda))) => #t)

  (check (path? (run-get 'echo-cmd)) => #t)
  (check (run-get 'not-exist) => #f)

  (run-set! 'custom-lambda (lambda () (display "ok\n")))
  (check (procedure? (run-get 'custom-lambda)) => #t)

  ;; run-allow! tests
  (run-allow! 'echo-cmd)
  (check (zero? (run '(echo-cmd "hello"))) => #t)
  (check-catch 'value-error (run '(my-lambda)))

  (run-allow! '())
  (check (zero? (run '(my-lambda))) => #t)

  ;; run-ban! tests
  (run-ban! 'echo-cmd)
  (check-catch 'value-error (run '(echo-cmd "hello")))
  (run-unban! 'echo-cmd)
) ;when

;; cross-platform process tests
(let ((proc (process-spawn '("python3" "-c" "import sys; sys.exit(0)"))))
  (check (process? proc) => #t)
  (check (real? (process-last-activity-time proc)) => #t)
  (check (>= (process-idle-seconds proc) 0) => #t)
  (check (process-wait proc 2000) => 0)
  (process-close proc)
) ;let

;; process-kill-tree regression tests
(let ((proc (process-spawn '("python3" "-c" "import time; time.sleep(10)"))))
  (check (process-alive? proc) => #t)
  (check (process-kill-tree proc) => #t)
  (check (integer? (process-wait proc 2000)) => #t)
  (check (process-alive? proc) => #f)
  (process-close proc)
) ;let
(check (process-kill-tree 999999) => #t)
(check-catch 'value-error (process-kill-tree 0))
(check-catch 'type-error (process-kill-tree "not-a-process"))

;; cross-platform idle-timeout tests
(let ((ret (run-streaming '("python3" "-c" "import time; time.sleep(5)")
             :timeout      10
             :idle-timeout 0.3
           ) ;run-streaming
      ) ;ret
     ) ;
  (check ret => #f)
) ;let

(check-report)
