;; anthropic-parse-tool-arguments
;; 解析响应中的 tool_use input，统一转换为 Scheme 内部使用的 arguments alist。
;;
;; 语法
;; ----
;; (anthropic-parse-tool-arguments raw)
;;
;; 参数
;; ----
;; raw : alist | string? | #f | null
;; 响应中的 tool_use input，支持 alist、JSON 字符串与空值。
;;
;; 返回值
;; ----
;; alist
;; 内部 arguments alist；空值或无法解析时返回空列表。

(import (liii check)
  (liii pi ai api anthropic-messages core)
) ;import

(check-set-mode! 'report-failed)

;; 空对象/空值返回 '()
(check (anthropic-parse-tool-arguments '()) => '())
(check (anthropic-parse-tool-arguments '(())) => '())
(check (anthropic-parse-tool-arguments #f) => '())
(check (anthropic-parse-tool-arguments 'null) => '())
(check (anthropic-parse-tool-arguments "") => '())
(check (anthropic-parse-tool-arguments "  ") => '())
(check (anthropic-parse-tool-arguments "{}") => '())

;; JSON 字符串参数反序列化
(let ((res (anthropic-parse-tool-arguments "{\"command\": \"ls -la\"}")))
  (check (cdr (assoc "command" res)) => "ls -la")
) ;let

;; 正常 alist 直接透传/规范化
(let ((res (anthropic-parse-tool-arguments '(("path" . "/tmp")))))
  (check (cdr (assoc "path" res)) => "/tmp")
) ;let

;; 畸形 JSON 字符串回退为 '()
(check (anthropic-parse-tool-arguments "not-a-json") => '())

(check-report)
