;; openai-build-tool-declarations-from-names
;; 根据内置工具名列表构建 OpenAI function calling 声明。
;;
;; 语法
;; ----
;; (openai-build-tool-declarations-from-names active-tool-names tool-resolver)
;;
;; 参数
;; ----
;; active-tool-names : list
;; 允许模型调用的工具名列表。
;; tool-resolver : procedure
;; 工具名到工具定义的解析函数，返回 #f 表示该工具当前不可用。
;;
;; 返回值
;; ----
;; list
;; OpenAI 格式的 tools 声明列表。

(import (liii check) (liii pi ai event) (liii pi ai api openai-completions) (liii pi code core tools))

(check-set-mode! 'report-failed)

(let ((decls (openai-build-tool-declarations-from-names '("bash" "read") get-tool-definition)))
  (check (length decls) => 2)
  (let* ((d1 (car decls))
         (fn1 (cdr (assoc "function" d1))))
    (check (cdr (assoc "type" d1)) => "function")
    (check (cdr (assoc "name" fn1)) => "bash")))

(check-report)
