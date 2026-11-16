;; json->pi-message
;; 将 (liii json) 解析出的 Scheme-JSON 对象转换为消息 record。
;;
;; 语法
;; ----
;; (json->pi-message obj)
;;
;; 参数
;; ----
;; obj : any?
;; Scheme-JSON alist 或已有的 pi-message record。
;;
;; 返回值
;; ----
;; any?
;; 对应的 <pi-user-message>、<pi-assistant-message>、<pi-tool-result-message> 或 #f。

(import (liii check) (liii json) (liii pi ai message))

(check-set-mode! 'report-failed)

(check-true (procedure? json->pi-message))

;; 1. 幂等：已是 pi-message 直接返回
(let ((m (pi-user-message "hi" 100)))
  (check (json->pi-message m) => m)
) ;let

;; 2. user message round-trip（文本与 block 数组）
(let* ((m1 (pi-user-message "hello" 1000))
       (r1
         (json->pi-message (string->json (json->string (pi-message->json m1))))
       ) ;r1
       (m2 (pi-user-message (list (pi-text-block "hi")) 2000))
       (r2
         (json->pi-message (string->json (json->string (pi-message->json m2))))
       ) ;r2
      ) ;
  (check-true (pi-user-message? r1))
  (check (pi-user-message-content r1) => "hello")
  (check (pi-user-message-timestamp r1) => 1000)
  (check-true (pi-user-message? r2))
  (check-true (vector? (pi-user-message-content r2)))
  (check-true (pi-text-block? (vector-ref (pi-user-message-content r2) 0)))
  (check (pi-text-block-text (vector-ref (pi-user-message-content r2) 0)) => "hi")
) ;let*

;; 3. assistant message round-trip
(let* ((m (pi-assistant-message (list (pi-text-block "ans") (pi-tool-call-block "c1" "bash" '()))
            :stop-reason   "toolUse"
            :error-message #f
            :usage         '(("input" . 10) ("output" . 20))
            :timestamp     3000
            :provider      "google"
            :model         "gemini"
            :api           "google-generative-ai"
          ) ;pi-assistant-message
       ) ;m
       (r
         (json->pi-message (string->json (json->string (pi-message->json m))))
       ) ;r
      ) ;
  (check-true (pi-assistant-message? r))
  (check (pi-assistant-message-stop-reason r) => "toolUse")
  (check-false (pi-assistant-message-error-message r))
  (check (json-ref (pi-assistant-message-usage r) "input") => 10)
  (check (pi-assistant-message-timestamp r) => 3000)
  (check (pi-assistant-message-provider r) => "google")
  (check (pi-assistant-message-model r) => "gemini")
  (check (pi-assistant-message-api r) => "google-generative-ai")
  (check-true (pi-tool-call-block? (vector-ref (pi-assistant-message-content r) 1))
  ) ;check-true
) ;let*

;; 4. toolResult message round-trip（含 JSON true/false 归一）
(let* ((m (pi-tool-result-message "c1" "bash" (vector (pi-text-block "ok")) #t 4000))
       (r
         (json->pi-message (string->json (json->string (pi-message->json m))))
       ) ;r
      ) ;
  (check-true (pi-tool-result-message? r))
  (check (pi-tool-result-message-tool-call-id r) => "c1")
  (check (pi-tool-result-message-tool-name r) => "bash")
  (check (pi-tool-result-message-is-error r) => #t)
  (check (pi-tool-result-message-timestamp r) => 4000)
  (check-true (pi-text-block? (vector-ref (pi-tool-result-message-content r) 0)))
) ;let*

;; 5. 未知 role 返回 #f，非标准 JSON 对象（含 symbol-key alist）抛出 type-error
(check-false (json->pi-message '(("role" . "system"))))
(check-catch 'type-error (json->pi-message '((role . "user") (content . "hi"))))
(check-catch 'type-error (json->pi-message 42))

(check-report)
