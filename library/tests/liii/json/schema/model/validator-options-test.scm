;; validator-options
;; 读取校验器记录中的选项。
;;
;; 语法
;; ----
;; (validator-options validator)
;;
;; 参数
;; ----
;; validator : validator
;; 校验器记录。
;;
;; 返回值
;; ----
;; any?
;; 校验器选项，通常是 alist。
;;
;; 注意
;; ----
;; 选项以 alist 形式保存，键为符号，
;; 例如 (max-errors . 10)、(registry . <登记表>)。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema model))

(check-set-mode! 'report-failed)

(define options '((max-errors . 10) (max-depth . 5)))

(define validator (make-validator '(("type" . "integer")) options))

(check (validator-options validator) => options)

;; 空选项也可以。

(check (validator-options (make-validator #t '())) => '())

(check-report)
