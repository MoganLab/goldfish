;; http-require-boolean
;; 校验必填布尔参数，非布尔时抛出 type-error。
;;
;; 语法
;; ----
;; (http-require-boolean who field value)
;;
;; 参数
;; ----
;; who   : string?  调用方函数名称，用于错误信息前缀。
;; field : string?  参数字段名称。
;; value : any      待校验的参数值。
;;
;; 返回值
;; ----
;; boolean?
;; value 为布尔时原样返回。
;;
;; 错误处理
;; ----
;; type-error
;; value 不是布尔时抛出。

(import (liii check) (liii http-common))

(check-set-mode! 'report-failed)

;; 合法布尔原样返回
(check (http-require-boolean "test" "stream" #t) => #t)
(check (http-require-boolean "test" "stream" #f) => #f)

;; 非布尔抛出 type-error
(check-catch 'type-error (http-require-boolean "test" "stream" 1))
(check-catch 'type-error (http-require-boolean "test" "stream" "true"))
(check-catch 'type-error (http-require-boolean "test" "stream" 'symbol))
(check-catch 'type-error (http-require-boolean "test" "stream" '()))

(check-report)
