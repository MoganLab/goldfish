;; check-report
;; 输出测试汇总信息，并在存在失败断言时退出进程。
;;
;; 语法
;; ----
;; (check-report [msg])
;;
;; 参数
;; ----
;; msg : string? (可选)
;; 输出测试汇总前打印的可选前缀提示信息。
;;
;; 返回值
;; ----
;; unspecified
;; 若无失败用例则输出汇总并返回；若有失败用例则以状态码 -1 退出进程。

(import (scheme base) (liii check) (liii subprocess) (liii either) (liii sys))

(check-set-mode! 'report-failed)

;; 1. 无失败用例时正常调用与打印消息
(check 1 => 1)
(check-report "All checks passed:\n")

;; 2. 失败时以非零状态码退出进程
(let ((res (run-either (list (executable) "-e" "(import (liii check))\n(check 1 => 2)\n(check-report)")
             :stdout 'capture
             :stderr 'capture
           ) ;run-either
      ) ;res
     ) ;
  (check-true (either-left? res))
  (check-true
    (not (zero? (process-error-code (to-left res))))
  ) ;check-true
) ;let

(check-report)
