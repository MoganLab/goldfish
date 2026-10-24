(import (liii check)
  (liii pi ai api anthropic-messages input)
) ;import

(check-set-mode! 'report-failed)

;; anthropic-parse-tool-arguments
;; 解析响应中的 tool_use input，统一转换为 Scheme 内部使用的 arguments alist。

;; 1. 空对象/空值返回 '()
(check (anthropic-parse-tool-arguments '()) => '())
(check (anthropic-parse-tool-arguments '(())) => '())
(check (anthropic-parse-tool-arguments #f) => '())
(check (anthropic-parse-tool-arguments 'null) => '())
(check (anthropic-parse-tool-arguments "") => '())
(check (anthropic-parse-tool-arguments "  ") => '())
(check (anthropic-parse-tool-arguments "{}") => '())

;; 2. JSON 字符串参数反序列化
(let ((res (anthropic-parse-tool-arguments "{\"command\": \"ls -la\"}")))
  (check (cdr (assoc "command" res)) => "ls -la")
) ;let

;; 3. 正常 alist 直接透传/规范化
(let ((res (anthropic-parse-tool-arguments '(("path" . "/tmp")))))
  (check (cdr (assoc "path" res)) => "/tmp")
) ;let

;; 4. 畸形 JSON 字符串回退为 '()
(check (anthropic-parse-tool-arguments "not-a-json") => '())

(check-report)
