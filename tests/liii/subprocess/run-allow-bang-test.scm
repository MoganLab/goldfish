(import (liii check) (liii os) (liii subprocess))

;; run-allow!
;; 设置符号命令白名单。
;;
;; 语法
;; ----
;; (run-allow! symbol)
;; (run-allow! symbol-list)
;;
;; 参数
;; ----
;; symbol 或 symbol-list : 单个符号或符号列表。
;; 空列表表示取消白名单。

(when (os-linux?)
  (run-set! 'echo-cmd "/bin/echo")
  (run-set! 'true-cmd "/bin/true")

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
)

(check-report)
