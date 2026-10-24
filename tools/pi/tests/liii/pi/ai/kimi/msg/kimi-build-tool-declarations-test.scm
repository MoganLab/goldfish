(import (liii check) (liii pi ai event) (liii pi ai kimi) (liii pi ai tools record))

(check-set-mode! 'report-failed)

;; kimi-build-tool-declarations
;; 将内部 tool-definition records 转换为 OpenAI function calling 格式。
(let* ((defs (list (pi-tool-definition "bash" "Run bash command"
                     '(("type" . "object")
                       ("properties" . (("command" . (("type" . "string")))))
                       ("required" . #("command"))))))
       (decls (kimi-build-tool-declarations defs)))
  (check (length decls) => 1)
  (let* ((item (car decls))
         (fn (cdr (assoc "function" item))))
    (check (cdr (assoc "type" item)) => "function")
    (check (cdr (assoc "name" fn)) => "bash")
    (check (cdr (assoc "description" fn)) => "Run bash command")
    (check (pair? (cdr (assoc "parameters" fn))) => #t)))

(check-report)
