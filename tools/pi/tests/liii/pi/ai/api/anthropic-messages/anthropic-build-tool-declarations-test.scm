(import (liii check) (liii pi ai api anthropic-messages) (liii pi ai event))

(check-set-mode! 'report-failed)

;; anthropic-build-tool-declarations
;; 构造符合 Anthropic 规格的 input_schema 工具声明。
(let* ((tools (list (list (cons 'name "read")
                          (cons 'description "Read file contents")
                          (cons 'parametersJsonSchema
                                '(("type" . "object")
                                  ("properties" . (("path" . (("type" . "string")))))
                                  ("required" . #("path")))))))
       (decls (anthropic-build-tool-declarations tools))
       (first-tool (car decls)))
  (check (cdr (assoc "name" first-tool)) => "read")
  (check (cdr (assoc "description" first-tool)) => "Read file contents")
  (check-true (pair? (cdr (assoc "input_schema" first-tool)))))

(check-report)
