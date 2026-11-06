(import (liii check) (liii http-common))

(check-set-mode! 'report-failed)

;; http-optional-timeout
;; 校验并返回可选的 HTTP 请求超时时间（秒）。
;;
;; 语法
;; ----
;; (http-optional-timeout who field value)
;;
;; 参数
;; ----
;; who   : string - 调用方函数名称，用于错误信息前缀
;; field : string - 参数字段名称，通常为 "timeout"
;; value : any    - 待校验的超时值；支持 #f、正整数或正浮点数
;;
;; 返回值
;; ----
;; real? | integer? | #f
;;   当 value 为 #f 时返回 #f；当 value 为合法正数时返回该数值本身。
;;
;; 错误处理
;; ----
;; type-error
;;   当 value 既不是 #f 也不是数字时抛出。
;; value-error
;;   当 value 为小于等于 0 的数字时抛出。

;; 值为 #f 时返回 #f
(check (http-optional-timeout "test" "timeout" #f) => #f)

;; 合法正整数
(check (http-optional-timeout "test" "timeout" 5) => 5)
(check (http-optional-timeout "test" "timeout" 1) => 1)

;; 合法正浮点数
(check (http-optional-timeout "test" "timeout" 0.5) => 0.5)
(check (http-optional-timeout "test" "timeout" 2.5) => 2.5)

;; 非数字非 #f 抛出 type-error
(check-catch 'type-error (http-optional-timeout "test" "timeout" "5"))
(check-catch 'type-error (http-optional-timeout "test" "timeout" 'fast))
(check-catch 'type-error (http-optional-timeout "test" "timeout" #t))
(check-catch 'type-error (http-optional-timeout "test" "timeout" '()))
(check-catch 'type-error (http-optional-timeout "test" "timeout" (vector 1)))

;; 非正数值抛出 value-error
(check-catch 'value-error (http-optional-timeout "test" "timeout" 0))
(check-catch 'value-error (http-optional-timeout "test" "timeout" 0.0))
(check-catch 'value-error (http-optional-timeout "test" "timeout" -1))
(check-catch 'value-error (http-optional-timeout "test" "timeout" -0.5))

(check-report)
