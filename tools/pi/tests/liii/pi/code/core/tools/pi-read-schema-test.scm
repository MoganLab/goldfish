(import (liii check) (liii string) (liii pi code core tools read) (liii pi code core session-manager))

(check-set-mode! 'report-failed)

;; pi-read-schema
;; 获取 read 工具的 JSON Schema 定义。
(let ((schema (pi-read-schema)))
  (check (cdr (assoc "type" schema)) => "object")
  (check-true (pair? (assoc "properties" schema)))
  (let ((desc (cdr (or (assoc "description" schema) '(#f . "")))))
    (check-true (string? desc))
    (check-true (string-contains? desc "jpg"))
    (check-true (string-contains? desc "png"))
    (check-true (string-contains? desc "attachments"))
  ) ;let
) ;let

(check-report)
