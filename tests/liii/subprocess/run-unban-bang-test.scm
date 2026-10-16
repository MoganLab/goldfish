(import (liii check) (liii os) (liii subprocess))

(check-set-mode! 'report-failed)

;; run-unban!
;; 取消对某个符号命令的禁止。
;;
;; 语法
;; ----
;; (run-unban! symbol)
;;
;; 参数
;; ----
;; symbol : symbol
;; 需要解除禁令的命令符号。
;;
;; 返回值
;; ----
;; 未定义
;;
;; 说明
;; ----
;; - 用于解除由 run-ban! 或默认设置的命令禁止。
;; - 解除后，该命令可以再次正常执行。

(when (or (os-linux?) (os-macos?))
  (run-set! 'unban-echo "/bin/echo")
  (run-ban! 'unban-echo)
  (check-catch 'value-error (run '(unban-echo "hello")))

  (run-unban! 'unban-echo)
  (check (zero? (run '(unban-echo "hello"))) => #t)
) ;when

(when (os-windows?)
  (run-set! 'unban-py "python3")
  (run-ban! 'unban-py)
  (check-catch 'value-error (run '(unban-py "-c" "pass")))

  (run-unban! 'unban-py)
  (check (zero? (run '(unban-py "-c" "pass"))) => #t)
) ;when

(check-report)
