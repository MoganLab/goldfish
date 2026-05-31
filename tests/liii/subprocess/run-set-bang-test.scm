(import (liii check) (liii os) (liii subprocess))

;; run-set!
;; 为符号命令注册解析方式。
;;
;; 语法
;; ----
;; (run-set! symbol value)
;;
;; 参数
;; ----
;; symbol : symbol
;; 要注册的符号命令名称。
;;
;; value : string 或 path 或 procedure
;; 字符串或 path 对象表示外部可执行文件路径；
;; lambda 表示自定义命令。
;;
;; 说明
;; ----
;; 注册表是全局的，作用域覆盖所有后续的 run / run-either / run-values 调用。

(when (os-linux?)
  (run-set! 'echo-cmd "/bin/echo")
  (check (zero? (run '(echo-cmd "hello"))) => #t)

  (run-set! 'true-cmd "/bin/true")
  (check (zero? (run '(true-cmd))) => #t)

  (run-set! 'my-lambda (lambda () (display "ok\n")))
  (check (zero? (run '(my-lambda))) => #t)
)

(check-report)
