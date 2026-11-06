;; json-schema-builtin-format-checkers
;; 返回内置格式检查器表。
;;
;; 语法
;; ----
;; (json-schema-builtin-format-checkers)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; list
;; 格式名与检查过程组成的 alist 列表。
;;
;; 注意
;; ----
;; 表中包含 ipv4、ipv6、email、idn-email、hostname、date、time、
;; date-time、duration、uri、uri-reference、json-pointer 与
;; relative-json-pointer。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format))

(check-set-mode! 'report-failed)

(define checkers (json-schema-builtin-format-checkers))

;; 内置表是一张 alist。

(check (pair? checkers) => #t)
(check (pair? (car checkers)) => #t)
(check (car (car checkers)) => 'ipv4)

;; 表中的每一项都是「格式名 . 检查过程」。

(define (lookup name)
  (cdr (assq name checkers))
) ;define

(check (procedure? (lookup 'ipv4)) => #t)
(check (procedure? (lookup 'ipv6)) => #t)
(check (procedure? (lookup 'email)) => #t)
(check (procedure? (lookup 'idn-email)) => #t)
(check (procedure? (lookup 'hostname)) => #t)
(check (procedure? (lookup 'date)) => #t)
(check (procedure? (lookup 'time)) => #t)
(check (procedure? (lookup 'date-time)) => #t)
(check (procedure? (lookup 'duration)) => #t)
(check (procedure? (lookup 'uri)) => #t)
(check (procedure? (lookup 'uri-reference)) => #t)
(check (procedure? (lookup 'json-pointer)) => #t)
(check (procedure? (lookup 'relative-json-pointer)) => #t)

;; 表中包含全部内置格式。

(check (length checkers) => 13)

;; 检查过程可以真正执行。

(check-true ((lookup 'date) "2020-01-01"))
(check-false ((lookup 'date) "2020-13-01"))
(check-true ((lookup 'ipv4) "192.168.0.1"))
(check-false ((lookup 'ipv4) "256.0.0.1"))

(check-report)
