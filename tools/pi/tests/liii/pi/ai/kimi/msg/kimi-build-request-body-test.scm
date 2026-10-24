(import (liii check) (liii pi ai event) (liii pi ai kimi) (liii pi ai message))

(check-set-mode! 'report-failed)

;; kimi-build-request-body
;; 组装 OpenAI Chat Completions 请求体 alist。
(let* ((msgs (vector '(("role" . "user") ("content" . "hi"))))
       (tools '((("type" . "function") ("function" . (("name" . "bash"))))))
       (body (kimi-build-request-body "kimi-for-coding" msgs #t tools 4096)))
  (check (cdr (assoc "model" body)) => "kimi-for-coding")
  (check (cdr (assoc "messages" body)) => msgs)
  (check (cdr (assoc "stream" body)) => #t)
  (check (cdr (assoc "max_tokens" body)) => 4096)
  (check (cdr (assoc "tools" body)) => (list->vector tools)))

;; 无 tools 和非流式
(let* ((msgs (vector '(("role" . "user") ("content" . "hi"))))
       (body (kimi-build-request-body "kimi-for-coding" msgs #f '() #f)))
  (check (cdr (assoc "model" body)) => "kimi-for-coding")
  (check (assoc "stream" body) => #f)
  (check (assoc "tools" body) => #f))

(check-report)
