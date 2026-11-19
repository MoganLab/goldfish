;; OpenAI 自定义请求头 HTTP 集成：Vertex OpenAI 兼容端点要求
;; x-goog-api-key，而非只接受 Authorization Bearer。
;;
;; 语法
;; ----
;; (openai-request config messages system-instruction tool-names timeout opt-retry)
;; (openai-request-stream config messages system-instruction tool-names on-event timeout opt-retry)
;;
;; 参数
;; ----
;; config : alist
;; 包含 model-id、model-meta、api-key、headers 与重试配置。
;;
;; 返回值
;; ----
;; pi-assistant-message?
;; 缺少认证头时返回错误消息；配置 ${API_KEY} 后，非流式与流式均返回成功文本。

(import (scheme base)
  (liii check)
  (liii string)
  (liii subprocess)
  (liii pi ai message)
  (liii pi ai api openai-completions)
) ;import

(check-set-mode! 'report-failed)

(define (find-stub-server)
  (let loop ((candidates '("library/tests/stub_vertex_openai_server.py"
                           "tests/stub_vertex_openai_server.py"
                           "stub_vertex_openai_server.py")))
    (cond ((null? candidates) (error 'file-not-found "stub_vertex_openai_server.py not found"))
          ((file-exists? (car candidates)) (car candidates))
          (else (loop (cdr candidates)))
    ) ;cond
  ) ;let
) ;define

(define (make-config base-url headers)
  (list (cons 'provider "google")
    (cons 'model-id "vertex-stub")
    (cons 'model-meta `(("base-url" . ,base-url) ("max-tokens" . 128)))
    (cons 'api-key "test-key")
    (cons 'headers headers)
    (cons 'proxy-url #f)
    (cons 'tools '())
    (cons 'settings '((retry (enabled . #f))))
  ) ;list
) ;define

(define (response-text resp)
  (pi-text-block-text (car (pi-assistant-message-content resp)))
) ;define

(define server (process-spawn (list "python3" (find-stub-server)) :stdout 'pipe))
(define port-line (process-read-line server))
(define port
  (and (string? port-line) (string-starts? port-line "PORT:")
    (string->number (substring port-line 5 (string-length port-line)))
  ) ;and
) ;define

(dynamic-wind
  (lambda () #t)
  (lambda ()
    (check-true (number? port))
    (when (number? port)
      (let ((base-url (string-append "http://127.0.0.1:" (number->string port))))
        ;; 缺少 x-goog-api-key：保留原有 401 错误结算。
        (let ((resp (openai-request (make-config base-url '()) (list (pi-user-message "ping"))
                      #f '() 10 #f
                    ) ;openai-request
              ) ;resp
             ) ;
          (check (pi-assistant-message-stop-reason resp) => "error")
          (check-true
            (string-contains? (pi-assistant-message-error-message resp) "API key not valid.")
          ) ;check-true
        ) ;let

        ;; 非流式：${API_KEY} 在 HTTP 请求时展开。
        (let ((resp (openai-request
                      (make-config base-url '(("x-goog-api-key" . "${API_KEY}")))
                      (list (pi-user-message "ping")) #f '() 10 #f
                    ) ;openai-request
              ) ;resp
             ) ;
          (check (pi-assistant-message-stop-reason resp) => "stop")
          (check (response-text resp) => "header-ok:test-key")
        ) ;let

        ;; 流式：同一 config.headers 链路生效。
        (let ((resp (openai-request-stream
                      (make-config base-url '(("x-goog-api-key" . "${API_KEY}")))
                      (list (pi-user-message "ping")) #f '() #f 10 #f
                    ) ;openai-request-stream
              ) ;resp
             ) ;
          (check (pi-assistant-message-stop-reason resp) => "stop")
          (check (response-text resp) => "header-ok:test-key")
        ) ;let
      ) ;let
    ) ;when
  ) ;lambda
  (lambda () (process-kill server))
) ;dynamic-wind

(check-report)
