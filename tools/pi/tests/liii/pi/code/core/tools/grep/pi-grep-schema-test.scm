;; pi-grep-schema
;; grep 工具的 JSON schema 定义
;;
;; 语法
;; ----
;; (pi-grep-schema)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; list?，工具 schema

(import (liii check)
  (liii pi code core tools grep)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; pi-grep-schema
;; 获取 grep 工具的 JSON Schema 定义。
(let ((schema (pi-grep-schema)))
  (check (cdr (assoc "type" schema)) => "object")
  (check-true (pair? (assoc "properties" schema)))
) ;let

(check-report)
