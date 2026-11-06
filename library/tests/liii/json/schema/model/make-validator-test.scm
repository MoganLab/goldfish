;; make-validator
;; 创建校验器记录。
;;
;; 语法
;; ----
;; (make-validator schema options)
;;
;; 参数
;; ----
;; schema : any?
;; 校验器使用的根 Schema。
;;
;; options : any?
;; 校验器选项，通常是 alist。
;;
;; 返回值
;; ----
;; validator
;; 校验器记录。
;;
;; 注意
;; ----
;; 校验器记录是不可变的，
;; 记录本身不持有校验过程中的可变状态。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema model))

(check-set-mode! 'report-failed)

(define validator (make-validator '(("type" . "integer")) '()))

;; 创建的是合法的校验器记录。

(check-true (json-schema-validator? validator))
(check (validator-schema validator) => '(("type" . "integer")))
(check (validator-options validator) => '())

;; 每次创建得到相互独立的记录。

(check
  (eq? validator (make-validator '(("type" . "integer")) '()))
  =>
  #f
) ;check

(check-report)
