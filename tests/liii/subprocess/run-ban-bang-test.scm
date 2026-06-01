(import (liii check) (liii os) (liii subprocess))

;; run-ban!
;; 显式禁止某个符号命令或字符串命令。
;;
;; 语法
;; ----
;; (run-ban! symbol)
;;
;; 参数
;; ----
;; symbol : symbol
;;
;; 说明
;; ----
;; - 'rm 默认已被禁止。
;; - 禁止符号命令后，该符号无法通过 run/run-values 等函数执行。
;; - 对于字符串命令，如果命令以被禁止的符号名称开头（后跟空格或结束），
;;   同样会报 value-error。
;; - 使用 (run-unban! symbol) 可以取消禁止。

(when (os-linux?)
  ;; 符号命令禁止与取消
  (run-set! 'echo-cmd "/bin/echo")
  (run-ban! 'echo-cmd)
  (check-catch 'value-error (run '(echo-cmd "hello")))

  (run-unban! 'echo-cmd)
  (check (zero? (run '(echo-cmd "hello"))) => #t)

  ;; 'rm 默认被禁止（字符串形式）
  (check-catch 'value-error (run "rm -rf /tmp/test"))
  (check-catch 'value-error (run "rm"))

  ;; 但 rmdir 不受影响
  (check (zero? (run "true")) => #t)
) ;when

(when (os-windows?)
  ;; 符号命令禁止与取消
  (run-set! 'pytrue "python3")
  (run-ban! 'pytrue)
  (check-catch 'value-error (run '(pytrue "-c" "pass")))

  (run-unban! 'pytrue)
  (check (zero? (run '(pytrue "-c" "pass"))) => #t)

  ;; 'rm 默认被禁止（字符串形式）
  (check-catch 'value-error (run "rm -rf /tmp/test"))
  (check-catch 'value-error (run "rm"))

  ;; 其他命令不受影响
  (check (zero? (run "python3 -c pass")) => #t)
) ;when

(check-report)
