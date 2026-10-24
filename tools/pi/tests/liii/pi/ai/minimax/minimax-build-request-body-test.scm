(import (liii check) (liii pi ai minimax) (liii pi code tools))

(check-set-mode! 'report-failed)

;; minimax-build-request-body
;; 构造发往 MiniMax Anthropic 兼容端点的请求体 JSON。
(let* ((msgs (list '((role . "user") (content . "Hello MiniMax"))))
       (tool-decls (minimax-build-tool-declarations-from-names '("read") get-tool-definition))
       (body (minimax-build-request-body "MiniMax-M3.1-Flash" msgs "You are a bot" tool-decls 4096)))
  (check (cdr (assoc "model" body)) => "MiniMax-M3.1-Flash")
  (check (cdr (assoc "max_tokens" body)) => 4096)
  (check (cdr (assoc "system" body)) => "You are a bot")
  (let ((tools-val (cdr (assoc "tools" body))))
    (check (vector? tools-val) => #t)
    (check (> (vector-length tools-val) 0) => #t)))

(check-report)
