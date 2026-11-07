;; pi-write-schema
;; write 工具的 JSON schema 定义
;;
;; 语法
;; ----
;; (pi-write-schema)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; list?，工具 schema

(import (liii check)
  (liii pi code core session-manager)
  (liii pi code core tools)
) ;import

(check-set-mode! 'report-failed)

;; pi-write-schema
;; 获取 write 工具的 JSON Schema 定义。
(let ((schema (pi-write-schema)))
  (check (cdr (assoc "type" schema)) => "object")
  (check-true (pair? (assoc "properties" schema)))
) ;let

(check-report)
