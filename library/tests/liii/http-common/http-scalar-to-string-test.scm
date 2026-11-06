;; http-scalar->string
;; 将字符串、符号或数字等标量值转换为字符串表示。
;;
;; 语法
;; ----
;; (http-scalar->string who field value)
;;
;; 参数
;; ----
;; who   : string?  调用方函数名称，用于错误信息前缀。
;; field : string?  参数字段名称。
;; value : any      待转换的标量值，支持字符串、符号与数字。
;;
;; 返回值
;; ----
;; string?
;; 标量值对应的字符串表示。
;;
;; 错误处理
;; ----
;; type-error
;; value 不是字符串、符号或数字时抛出。

(import (liii check) (liii http-common))

(check-set-mode! 'report-failed)

;; 字符串原样返回
(check (http-scalar->string "test" "value" "") => "")
(check (http-scalar->string "test" "value" "abc") => "abc")

;; 符号转字符串
(check (http-scalar->string "test" "value" 'key) => "key")
(check (http-scalar->string "test" "value" 'Content-Type) => "Content-Type")

;; 整数与实数转字符串
(check (http-scalar->string "test" "value" 0) => "0")
(check (http-scalar->string "test" "value" 42) => "42")
(check (http-scalar->string "test" "value" -7) => "-7")

;; 非字符串/符号/数字抛出 type-error
(check-catch 'type-error (http-scalar->string "test" "value" #t))
(check-catch 'type-error (http-scalar->string "test" "value" #f))
(check-catch 'type-error (http-scalar->string "test" "value" '()))
(check-catch 'type-error (http-scalar->string "test" "value" (list 1 2)))
(check-catch 'type-error (http-scalar->string "test" "value" (vector 1)))

(check-report)
