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

(import (liii check)
  (liii pi ai api anthropic-messages core)
  (liii pi ai event)
  (liii pi ai tools record)
) ;import

(check-set-mode! 'report-failed)

;; alist 形态的工具定义
(let* ((tools (list (list (cons 'name "read")
                          (cons 'description "Read file contents")
                          (cons 'parametersJsonSchema
                                '(("type" . "object")
                                  ("properties" . (("path" . (("type" . "string")))))
                                  ("required" . #("path")))))))
       (decls (anthropic-build-tool-declarations tools))
       (first-tool (car decls)))
  (check (length decls) => 1)
  (check (cdr (assoc "name" first-tool)) => "read")
  (check (cdr (assoc "description" first-tool)) => "Read file contents")
  (check-true (pair? (cdr (assoc "input_schema" first-tool))))
) ;let*

;; 缺少参数 schema 时退化为空对象 schema
(let* ((tools (list (list (cons 'name "bash") (cons 'description "Run shell"))))
       (decls (anthropic-build-tool-declarations tools))
       (first-tool (car decls)))
  (check (cdr (assoc "name" first-tool)) => "bash")
  (check (cdr (assoc "input_schema" first-tool)) => '(("type" . "object")))
) ;let*

;; record 形态的工具定义
(let* ((tool (pi-tool-definition "read" "Read file contents"
                    '(("type" . "object") ("properties" . (("path" . (("type" . "string"))))))
                    (lambda (args) "ok")
                  ))
       (decls (anthropic-build-tool-declarations (list tool)))
       (first-tool (car decls)))
  (check (cdr (assoc "name" first-tool)) => "read")
  (check (cdr (assoc "description" first-tool)) => "Read file contents")
  (check-true (pair? (cdr (assoc "input_schema" first-tool))))
) ;let*

;; 空工具列表
(check (anthropic-build-tool-declarations '()) => '())

(check-report)
