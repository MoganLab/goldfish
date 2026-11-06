;; make-json-schema-registry
;; 创建空的 JSON Schema 资源登记表。
;;
;; 语法
;; ----
;; (make-json-schema-registry)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; registry
;; 空的资源登记表。
;;
;; 注意
;; ----
;; 登记表是不可变的，
;; 新增资源会返回新的登记表，原登记表保持不变。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema registry) (liii json schema model))

(check-set-mode! 'report-failed)

(define registry (make-json-schema-registry))

;; 新建的登记表是合法的登记表且不含任何资源。

(check-true (json-schema-registry? registry))
(check (registry-resources registry) => '())

;; 每次创建得到相互独立的登记表。

(check (eq? registry (make-json-schema-registry)) => #f)

(check-report)
