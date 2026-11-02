;; anthropic-build-tool-declarations
;; 构造符合 Anthropic 规格的 input_schema 工具声明。
;;
;; 语法
;; ----
;; (anthropic-build-tool-declarations tool-defs)
;;
;; 参数
;; ----
;; tool-defs : list
;; 工具定义列表，支持 pi-tool-definition record 与普通 alist。
;;
;; 返回值
;; ----
;; list
;; Anthropic API 的 tools 声明列表，每项包含 name、description 与 input_schema。

(import (liii check) (liii pi ai api anthropic-messages) (liii pi ai event))

(check-set-mode! 'report-failed)

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
