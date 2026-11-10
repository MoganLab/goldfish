;; http-require-procedure
;; 校验必填过程参数，非过程时抛出 type-error。
;;
;; 语法
;; ----
;; (http-require-procedure who field value)
;;
;; 参数
;; ----
;; who   : string?  调用方函数名称，用于错误信息前缀。
;; field : string?  参数字段名称。
;; value : any      待校验的参数值。
;;
;; 返回值
;; ----
;; procedure?
;; value 为过程时原样返回。
;;
;; 错误处理
;; ----
;; type-error
;; value 不是过程时抛出。

(import (liii check) (liii http common))

(check-set-mode! 'report-failed)

;; 合法过程原样返回
(check-true (procedure? (http-require-procedure "test" "callback" (lambda (x) x))))
(check-true (procedure? (http-require-procedure "test" "callback" car)))

;; 非过程抛出 type-error
(check-catch 'type-error (http-require-procedure "test" "callback" 42))
(check-catch 'type-error (http-require-procedure "test" "callback" "not-a-procedure"))
(check-catch 'type-error (http-require-procedure "test" "callback" 'symbol))
(check-catch 'type-error (http-require-procedure "test" "callback" #f))
(check-catch 'type-error (http-require-procedure "test" "callback" (vector car)))

(check-report)
