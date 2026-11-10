;; http-optional-procedure
;; 校验可选过程参数，#f 视为未提供。
;;
;; 语法
;; ----
;; (http-optional-procedure who field value)
;;
;; 参数
;; ----
;; who   : string?  调用方函数名称，用于错误信息前缀。
;; field : string?  参数字段名称。
;; value : any      待校验的参数值，允许 #f。
;;
;; 返回值
;; ----
;; procedure? | #f
;; value 为 #f 时返回 #f；为过程时原样返回。
;;
;; 错误处理
;; ----
;; type-error
;; value 为既非 #f 又非过程的值时抛出。

(import (liii check) (liii http common))

(check-set-mode! 'report-failed)

;; #f 表示未提供，返回 #f
(check (http-optional-procedure "test" "callback" #f) => #f)

;; 合法过程原样返回
(check-true (procedure? (http-optional-procedure "test" "callback" (lambda (c) #t))))

;; 非 #f 且非过程抛出 type-error
(check-catch 'type-error (http-optional-procedure "test" "callback" 42))
(check-catch 'type-error (http-optional-procedure "test" "callback" "chunk-handler"))
(check-catch 'type-error (http-optional-procedure "test" "callback" 'symbol))
(check-catch 'type-error (http-optional-procedure "test" "callback" '()))

(check-report)
