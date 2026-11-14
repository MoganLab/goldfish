;; pi-message->json
;; 将消息 record 转换为符合 (liii json) 规范的 Scheme-JSON 对象。
;;
;; 语法
;; ----
;; (pi-message->json msg)
;;
;; 参数
;; ----
;; msg : any?
;; <pi-user-message>、<pi-assistant-message>、<pi-tool-result-message> 或 alist。
;;
;; 返回值
;; ----
;; any?
;; 可直接用于 json->string 或 json-ref 的 Scheme-JSON 对象。

(import (liii check) (liii json) (liii pi ai message record))

(check-set-mode! 'report-failed)

(check-true (procedure? pi-message->json))

;; 1. user message（文本内容与 block 列表内容）
(let ((j1 (pi-message->json (pi-user-message "hello" 1000)))
      (j2 (pi-message->json (pi-user-message (list (pi-text-block "hi")) 2000)))
     ) ;
  (check (json-ref j1 "role") => "user")
  (check (json-ref j1 "content") => "hello")
  (check (json-ref j1 "timestamp") => 1000)
  (check (string->json (json->string j1)) => j1)
  (check-true (vector? (json-ref j2 "content")))
  (check (json-ref (vector-ref (json-ref j2 "content") 0) "text") => "hi")
) ;let

;; 2. assistant message：errorMessage=#f 被过滤，嵌套 block 与 usage 规范化
(let ((j (pi-message->json
           (pi-assistant-message (list (pi-text-block "ans")
                                   (pi-tool-call-block "c1" "bash" '())
                                 ) ;list
             :stop-reason   'toolUse
             :error-message #f
             :usage         '((input . 10) (output . 20))
             :timestamp     3000
             :provider      "google"
             :model         "gemini"
             :api           "google-generative-ai"
           ) ;pi-assistant-message
         ) ;pi-message->json
      ) ;j
     ) ;
  (check (json-ref j "role") => "assistant")
  (check (json-ref j "stopReason") => "toolUse")
  (check-false (json-contains-key? j "errorMessage"))
  (check (json-ref j "usage" "input") => 10)
  (check (json-ref j "provider") => "google")
  (check (json-ref (vector-ref (json-ref j "content") 1) "arguments") => '(()))
  (check (string->json (json->string j)) => j)
) ;let

;; 3. toolResult message
(let ((j (pi-message->json
           (pi-tool-result-message "c1" "bash" (vector (pi-text-block "ok")) #t 4000)
         ) ;pi-message->json
      ) ;j
     ) ;
  (check (json-ref j "role") => "toolResult")
  (check (json-ref j "toolCallId") => "c1")
  (check (json-ref j "toolName") => "bash")
  (check (json-ref j "isError") => #t)
  (check (json-ref (vector-ref (json-ref j "content") 0) "text") => "ok")
) ;let

(check-report)
