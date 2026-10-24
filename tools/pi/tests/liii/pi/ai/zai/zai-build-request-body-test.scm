(import (liii check) (liii pi ai zai) (liii pi code tools))

(check-set-mode! 'report-failed)

;; zai-build-request-body
;; 构造发往 ZAI Chat Completions 兼容端点的请求体 JSON。
(let* ((msgs (list (list (cons "role" "user") (cons "content" "Hello ZAI"))))
       (tool-decls (zai-build-tool-declarations-from-names '("bash") get-tool-definition))
       (body (zai-build-request-body "glm-5.3-flash" msgs #t tool-decls 4096))
      ) ;
  (check (cdr (assoc "model" body)) => "glm-5.3-flash")
  (check (cdr (assoc "max_tokens" body)) => 4096)
  (check (cdr (assoc "stream" body)) => #t)
  (check-true (pair? (assoc "tools" body)))
) ;let*

(check-report)
