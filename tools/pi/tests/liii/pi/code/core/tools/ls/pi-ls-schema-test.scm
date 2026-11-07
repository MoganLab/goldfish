;; pi-ls-schema
;; ls 工具的 JSON schema 定义
;;
;; 语法
;; ----
;; (pi-ls-schema)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; list?，工具 schema

(import (liii check)
  (liii pi code core tools ls)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; pi-ls-schema
;; 获取 ls 工具的 JSON Schema 定义。
(let ((schema (pi-ls-schema)))
  (check (cdr (assoc "type" schema)) => "object")
  (check-true (pair? (assoc "properties" schema)))
) ;let

(check-report)
