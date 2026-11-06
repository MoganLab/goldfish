;; json-schema-format-builtins
;; 内置格式检查器表。
;;
;; 语法
;; ----
;; (json-schema-format-builtins)
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
;; 表中的键是 format 关键字使用的名称，
;; 值是接受格式字符串并返回布尔值的检查过程。
;; 表本身是常量，调用方不应修改它。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-builtin))

(check-set-mode! 'report-failed)

;; 内置表是一张 alist。

(check (pair? json-schema-format-builtins) => #t)
(check (pair? (car json-schema-format-builtins)) => #t)
(check (car (car json-schema-format-builtins)) => 'ipv4)

;; 表中的每一项都是「格式名 . 检查过程」。

(define (lookup name)
  (cdr (assq name json-schema-format-builtins))
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

(check (length json-schema-format-builtins) => 13)

;; 检查过程可以真正执行。

(check-true ((lookup 'date) "2020-01-01"))
(check-false ((lookup 'date) "2020-13-01"))
(check-true ((lookup 'email) "user@example.com"))
(check-false ((lookup 'email) "user@@example.com"))

(check-report)
