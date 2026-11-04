;; run-get
;; 查询符号命令的注册值。
;;
;; 语法
;; ----
;; (run-get symbol)
;;
;; 参数
;; ----
;; symbol : symbol
;; 待查询的命令符号。
;;
;; 返回值
;; ----
;; path 对象、lambda 或 #f。
;; 未注册时返回 #f。
;;
;; 说明
;; ----
;; 若注册值为字符串，查询时会自动转换为 path 对象返回。

(import (liii check) (liii os) (liii path) (liii subprocess))

(when (os-linux?)
  (run-set! 'ls "/bin/ls")
  (check (path? (run-get 'ls)) => #t)
  (check (run-get 'not-exist) => #f)

  (run-set! 'custom-lambda (lambda () (display "ok\n")))
  (check (procedure? (run-get 'custom-lambda)) => #t)
) ;when

(when (os-windows?)
  (run-set! 'pytrue "python3")
  (check (path? (run-get 'pytrue)) => #t)
  (check (run-get 'not-exist) => #f)

  (run-set! 'custom-lambda (lambda () (display "ok\n")))
  (check (procedure? (run-get 'custom-lambda)) => #t)
) ;when

(check-report)
