;; openai-build-tool-declarations
;; 将内部 tool-definition records 转换为 OpenAI function calling 格式。
;;
;; 语法
;; ----
;; (openai-build-tool-declarations tool-defs)
;;
;; 参数
;; ----
;; tool-defs : list
;; 工具定义列表，支持 pi-tool-definition record 与普通 alist。
;;
;; 返回值
;; ----
;; list
;; OpenAI 格式的 tools 声明列表，每项包含 type="function" 与 function 描述。

(import (liii check) (liii pi ai event) (liii pi ai api openai-completions) (liii pi ai tools record))

(check-set-mode! 'report-failed)

(let* ((defs (list (pi-tool-definition "bash" "Run bash command"
                     '(("type" . "object")
                       ("properties" . (("command" . (("type" . "string")))))
                       ("required" . #("command"))))))
       (decls (openai-build-tool-declarations defs)))
  (check (length decls) => 1)
  (let* ((item (car decls))
         (fn (cdr (assoc "function" item))))
    (check (cdr (assoc "type" item)) => "function")
    (check (cdr (assoc "name" fn)) => "bash")
    (check (cdr (assoc "description" fn)) => "Run bash command")
    (check (pair? (cdr (assoc "parameters" fn))) => #t)))

(check-report)
