;; goldfish-cmd
;; TODO: 补充 goldfish-cmd 的简要描述。
;;
;; 语法
;; ----
;; (goldfish-cmd ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii os)
  (liii path)
  (liii string)
  (liii goldtest-runner)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? goldfish-cmd))
(check-true (string-contains (goldfish-cmd "bignum/tests/liii/bignum/eq-test.scm")
              "-m r7rs+bignum"
            ) ;string-contains
) ;check-true
(check-true (string-contains (goldfish-cmd "r7rs/tests/scheme/base/abs-test.scm") "-m r7rs ")
) ;check-true
(check-true (string-contains (goldfish-cmd "tui/tests/liii/tui/editor-test.scm") "-m r7rs ")
) ;check-true
(check-false (string-contains (goldfish-cmd "tui/tests/liii/tui/editor-test.scm") "-I ")
) ;check-false
(check-true (string-contains (goldfish-cmd "tools/fmt/tests/liii/goldfmt/dir-test.scm")
              "-m r7rs -I tools/fmt "
            ) ;string-contains
) ;check-true
;; 进入 domain 目录后，当前 domain 也以绝对路径显式加入 load-path。
(check-true (string-contains (goldfish-cmd "tests/liii/goldtest-runner/goldfish-cmd-test.scm")
              "-I "
            ) ;string-contains
) ;check-true

;; ===== 外部项目：当前 domain 与传递依赖都加入 load-path =====
(define fixture-root
  (path->string
    (path-join (path-temp-dir) (string-append "goldtest-project-" (number->string (getpid))))
  ) ;path->string
) ;define
(define fixture-code (path->string (path-join fixture-root "code")))
(define fixture-agent (path->string (path-join fixture-root "agent")))
(define fixture-shared (path->string (path-join fixture-root "shared")))
(define fixture-config (path->string (path-join fixture-root "gfproject.json")))
(define fixture-old-cwd (getcwd))

(mkdir fixture-root)
(mkdir fixture-code)
(mkdir fixture-agent)
(mkdir fixture-shared)
(call-with-output-file fixture-config
  (lambda (p)
    (display
      (string-append
        "{\"domains\":{"
        "\"code\":{\"root\":\"code/\",\"depends-on\":[\"agent\"],\"mode\":\"r7rs\"},"
        "\"agent\":{\"root\":\"agent/\",\"depends-on\":[\"shared\"],\"mode\":\"r7rs\"},"
        "\"shared\":{\"root\":\"shared/\",\"depends-on\":[],\"mode\":\"r7rs\"}}}"
      ) ;string-append
      p
    ) ;display
  ) ;lambda
) ;call-with-output-file

(dynamic-wind
  (lambda () (chdir fixture-code))
  (lambda ()
    (let ((cmd (goldfish-cmd "tests/e2e/example-test.scm")))
      (check-true
        (string-contains cmd
          (string-append "-I " fixture-code " -I " fixture-agent " -I " fixture-shared " ")
        ) ;string-contains
      ) ;check-true
    ) ;let
  ) ;lambda
  (lambda ()
    (chdir fixture-old-cwd)
    (delete-file fixture-config)
    (rmdir fixture-code)
    (rmdir fixture-agent)
    (rmdir fixture-shared)
    (rmdir fixture-root)
  ) ;lambda
) ;dynamic-wind

(check-report)
