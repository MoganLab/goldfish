;; http-optional-string
;; 校验可选字符串参数，#f 视为未提供。
;;
;; 语法
;; ----
;; (http-optional-string who field value)
;;
;; 参数
;; ----
;; who   : string?  调用方函数名称，用于错误信息前缀。
;; field : string?  参数字段名称。
;; value : any      待校验的参数值，允许 #f。
;;
;; 返回值
;; ----
;; string? | #f
;; value 为 #f 时返回 #f；为字符串时原样返回。
;;
;; 错误处理
;; ----
;; type-error
;; value 为既非 #f 又非字符串的值时抛出。

(import (liii check) (liii http common))

(check-set-mode! 'report-failed)

;; #f 表示未提供，返回 #f
(check (http-optional-string "test" "output-file" #f) => #f)

;; 合法字符串原样返回
(check (http-optional-string "test" "output-file" "") => "")
(check (http-optional-string "test" "output-file" "/tmp/out.bin") => "/tmp/out.bin")

;; 非 #f 且非字符串抛出 type-error
(check-catch 'type-error (http-optional-string "test" "output-file" 42))
(check-catch 'type-error (http-optional-string "test" "output-file" 'symbol))
(check-catch 'type-error (http-optional-string "test" "output-file" '()))
(check-catch 'type-error (http-optional-string "test" "output-file" #t))

(check-report)
