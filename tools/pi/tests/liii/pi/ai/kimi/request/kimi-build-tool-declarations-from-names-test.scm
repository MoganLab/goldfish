(import (liii check) (liii pi ai event) (liii pi ai kimi) (liii pi code tools))

(check-set-mode! 'report-failed)

;; kimi-build-tool-declarations-from-names
;; 根据内置工具名列表构建 OpenAI function calling 声明。
(let ((decls (kimi-build-tool-declarations-from-names '("bash" "read") get-tool-definition)))
  (check (length decls) => 2)
  (let* ((d1 (car decls))
         (fn1 (cdr (assoc "function" d1))))
    (check (cdr (assoc "type" d1)) => "function")
    (check (cdr (assoc "name" fn1)) => "bash")))

(check-report)
