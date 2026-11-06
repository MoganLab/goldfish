;; make-registry
;; 创建资源登记表记录。
;;
;; 语法
;; ----
;; (make-registry resources)
;;
;; 参数
;; ----
;; resources : list
;; 资源列表，每一项是「URI . Schema」的 alist 单元。
;;
;; 返回值
;; ----
;; registry
;; 资源登记表记录。
;;
;; 注意
;; ----
;; 登记表记录是不可变的，
;; 记录的增删由 (liii json schema registry) 负责。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema model))

(check-set-mode! 'report-failed)

(define registry (make-registry '()))

;; 创建的是合法的登记表记录。

(check-true (json-schema-registry? registry))
(check (registry-resources registry) => '())

;; 带上初始资源创建。

(define populated (make-registry '(("urn:integer" ("type" . "integer")))))
(check (length (registry-resources populated)) => 1)

(check-report)
