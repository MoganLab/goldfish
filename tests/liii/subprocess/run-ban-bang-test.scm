(import (liii check) (liii os) (liii subprocess))

;; run-ban!
;; 显式禁止某个符号命令。
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
;; 禁止后该符号无法解析，执行时报 value-error。

(when (os-linux?)
  (run-set! 'echo-cmd "/bin/echo")
  (run-ban! 'echo-cmd)
  (check-catch 'value-error (run '(echo-cmd "hello")))
)

(check-report)
