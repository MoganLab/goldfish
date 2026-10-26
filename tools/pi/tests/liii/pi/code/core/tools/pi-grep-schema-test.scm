(import (liii check) (liii pi code core tools grep) (liii pi code core session-manager))

(check-set-mode! 'report-failed)

;; pi-grep-schema
;; 获取 grep 工具的 JSON Schema 定义。
(let ((schema (pi-grep-schema)))
  (check (cdr (assoc "type" schema)) => "object")
  (check-true (pair? (assoc "properties" schema))))

(check-report)
