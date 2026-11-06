;; http-require-string
;; 校验必填字符串参数，非字符串时抛出 type-error。
;;
;; 语法
;; ----
;; (http-require-string who field value)
;;
;; 参数
;; ----
;; who   : string?  调用方函数名称，用于错误信息前缀。
;; field : string?  参数字段名称。
;; value : any      待校验的参数值。
;;
;; 返回值
;; ----
;; string?
;; value 为字符串时原样返回。
;;
;; 错误处理
;; ----
;; type-error
;; value 不是字符串时抛出。

(import (liii check) (liii http-common))

(check-set-mode! 'report-failed)

;; 合法字符串原样返回
(check (http-require-string "test" "url" "") => "")
(check (http-require-string "test" "url" "https://example.com") => "https://example.com")

;; 非字符串抛出 type-error
(check-catch 'type-error (http-require-string "test" "url" 42))
(check-catch 'type-error (http-require-string "test" "url" 'symbol))
(check-catch 'type-error (http-require-string "test" "url" #f))
(check-catch 'type-error (http-require-string "test" "url" '()))
(check-catch 'type-error (http-require-string "test" "url" (list 1 2)))

(check-report)
