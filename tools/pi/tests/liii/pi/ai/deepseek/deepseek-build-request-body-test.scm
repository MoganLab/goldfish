(import (liii check) (liii pi ai deepseek) (liii pi tools))

(check-set-mode! 'report-failed)

;; deepseek-build-request-body
;; 构造发往 DeepSeek 兼容网关的请求体 JSON。
(let* ((msgs (list '((role . "user") (content . "Hello DeepSeek"))))
       (tool-decls (deepseek-build-tool-declarations-from-names '("read") get-tool-definition))
       (body (deepseek-build-request-body "deepseek-chat" msgs "You are a bot" tool-decls 4096)))
  (check (cdr (assoc "model" body)) => "deepseek-chat")
  (check (cdr (assoc "max_tokens" body)) => 4096)
  (check (cdr (assoc "system" body)) => "You are a bot")
  (let ((tools-val (cdr (assoc "tools" body))))
    (check (vector? tools-val) => #t)
    (check (> (vector-length tools-val) 0) => #t)))

(check-report)
