;; pi-edit-schema
;; edit 工具的 JSON schema 定义
;;
;; 语法
;; ----
;; (pi-edit-schema)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; list?，工具 schema

(import (liii check)
  (liii pi code core tools edit)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; pi-edit-schema
;; 获取 edit 工具的 JSON Schema 定义。
(let ((schema (pi-edit-schema)))
  (check (cdr (assoc "type" schema)) => "object")
  (check-true (pair? (assoc "properties" schema)))
) ;let

(check-report)
