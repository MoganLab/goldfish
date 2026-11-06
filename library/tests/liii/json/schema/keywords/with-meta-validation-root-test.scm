;; with-meta-validation-root
;; 在指定的元校验根资源下调用过程。
;;
;; 语法
;; ----
;; (with-meta-validation-root root thunk)
;;
;; 参数
;; ----
;; root : any?
;; 过程执行期间生效的元校验根资源。
;;
;; thunk : procedure
;; 无参过程，在该根资源下执行。
;;
;; 返回值
;; ----
;; any?
;; thunk 的返回值。
;;
;; 注意
;; ----
;; 过程执行前设置根资源，执行后恢复上一层设置，
;; 即使过程抛出异常也会恢复。
;;
;; 嵌套调用时内层根资源在内层过程中生效。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema keywords) (liii json schema model))

(check-set-mode! 'report-failed)

(define meta-root '(("type" . "object")))

(define other-root '(("type" . "array")))

;; 初始状态下没有元校验根资源。

(set-meta-validation-root! #f)
(check-false (meta-validation-root))

;; 过程执行期间根资源生效。

(check (with-meta-validation-root meta-root (lambda () (meta-validation-root)))
  =>
  meta-root
) ;check

;; 过程结束后恢复原设置。

(check-false (meta-validation-root))

;; 过程返回值原样传出。

(check (with-meta-validation-root meta-root (lambda () 'value)) => 'value)

;; 过程抛出异常时也会恢复。

(check-catch 'json-schema-options-error
  (with-meta-validation-root meta-root
    (lambda () (error 'json-schema-options-error "boom"))
  ) ;with-meta-validation-root
) ;check-catch

(check-false (meta-validation-root))

;; 嵌套调用时内层根资源在内层过程中生效。

(check
  (with-meta-validation-root meta-root
    (lambda ()
      (with-meta-validation-root other-root (lambda () (meta-validation-root)))
    ) ;lambda
  ) ;with-meta-validation-root
  =>
  other-root
) ;check

(check-report)
