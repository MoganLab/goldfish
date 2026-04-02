(import (liii check)
        (liii os)
        (liii path)
        (liii string)
        (liii sys)
) ;import

(check-set-mode! 'report-failed)

;; check-report
;; 输出测试汇总，并在存在失败断言时以非零状态退出。
;;
;; 语法
;; ----
;; (check-report [msg])
;;
;; 使用场景
;; ----
;; 1. 在测试文件末尾输出本轮测试汇总。
;; 2. 让 CI 或脚本通过退出码感知失败。
;; 3. 在汇总前附带一段简短前缀说明。

(define (run-shell-command command)
  (os-call (string-append "sh -c \"" command "\""))
) ;define

(define (cleanup-check-report-fixture base-root)
  (path-unlink (path-join base-root "pass.scm") #t)
  (path-unlink (path-join base-root "fail.scm") #t)
  (path-unlink (path-join base-root "pass.log") #t)
  (path-unlink (path-join base-root "fail.log") #t)
  (if (path-dir? base-root)
      (path-rmdir base-root)
      #f
  ) ;if
) ;define

(when (not (os-windows?))
  (let* ((base-root (path-join (path-temp-dir)
                               (string-append "check-report-"
                                              (number->string (getpid)))))
         (pass-script (path-join base-root "pass.scm"))
         (fail-script (path-join base-root "fail.scm"))
         (pass-log (path-join base-root "pass.log"))
         (fail-log (path-join base-root "fail.log")))
    (cleanup-check-report-fixture base-root)
    (mkdir (path->string base-root))
    (path-write-text pass-script
                     "(import (liii check))\n(check-set-mode! 'summary)\n(check (+ 1 1) => 2)\n(check-report \"PASS \")\n")
    (path-write-text fail-script
                     "(import (liii check))\n(check-set-mode! 'summary)\n(check (+ 1 1) => 3)\n(check-report)\n")
    (dynamic-wind
      (lambda ()
        (path-unlink pass-log #t)
        (path-unlink fail-log #t)
      ) ;lambda
      (lambda ()
        (let ((pass-status (run-shell-command (string-append (executable)
                                                             " -m r7rs "
                                                             (path->string pass-script)
                                                             " > "
                                                             (path->string pass-log)
                                                             " 2>&1")))
              (fail-status (run-shell-command (string-append (executable)
                                                             " -m r7rs "
                                                             (path->string fail-script)
                                                             " > "
                                                             (path->string fail-log)
                                                             " 2>&1"))))
          (check (zero? pass-status) => #t)
          (check-false (zero? fail-status))
          (check-true (string-contains? (path-read-text pass-log) "PASS "))
          (check-true (string-contains? (path-read-text pass-log) "1 correct, 0 failed"))
          (check-true (string-contains? (path-read-text fail-log) "0 correct, 1 failed"))
        ) ;let
      ) ;lambda
      (lambda ()
        (cleanup-check-report-fixture base-root)
      ) ;lambda
    ) ;dynamic-wind
  ) ;let*
) ;when

(check-report)
