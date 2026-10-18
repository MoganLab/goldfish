(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-write-schema
;; 获取 write 工具的 JSON Schema 定义。
(let ((schema (pi-write-schema)))
  (check (cdr (assoc "type" schema)) => "object")
  (check-true (pair? (assoc "properties" schema))))

(check-report)
