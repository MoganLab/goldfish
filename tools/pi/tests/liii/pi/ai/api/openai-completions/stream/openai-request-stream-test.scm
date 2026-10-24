(import (liii check) (liii pi ai api openai-completions) (liii pi ai message))

(check-set-mode! 'report-failed)

;; openai-request-stream 网络错误或不可达端点处理
(let* ((cfg (list (cons 'model-id "kimi-for-coding")
                  (cons 'model-meta '(("base-url" . "http://127.0.0.1:1")
                                     ("max-tokens" . 1024)))
                  (cons 'api-key "dummy-key")
                  (cons 'proxy-url #f)
                  (cons 'tools '())
                  (cons 'settings '((retry . ((enabled . #f)))))))
       (msgs (list (pi-user-message "ping")))
       (resp (openai-request-stream cfg msgs #f '() #f 10)))
  (check (pi-assistant-message? resp) => #t)
  (check (pi-assistant-message-stop-reason resp) => "error")
  (check (string? (pi-assistant-message-error-message resp)) => #t))

(check-report)
