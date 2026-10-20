(import (liii check) (liii pi) (liii pi zai))

(check-set-mode! 'report-failed)

;; zai-parse-response
;; 解析 ZAI / OpenAI 兼容端点返回的完整响应 JSON 对象。
(let* ((resp '(("id" . "chatcmpl_123")
               ("object" . "chat.completion")
               ("choices"
                . #(((index . 0)
                     ("message"
                      ("role" . "assistant")
                      ("content" . "Hello from ZAI")
                      ("tool_calls"
                       . #((("id" . "call_1")
                            ("type" . "function")
                            ("function"
                             ("name" . "bash")
                             ("arguments" . "{\"command\":\"ls\"}")))
                         ) ;#
                      ))
                     ("finish_reason" . "tool_calls"))
                  ) ;#
               )
               ("usage"
                ("prompt_tokens" . 10)
                ("completion_tokens" . 5)
                ("total_tokens" . 15)))
       ) ;resp
       (parsed (zai-parse-response resp))
      ) ;
  (check (pi-assistant-message? parsed) => #t)
  (check (pi-assistant-message-role parsed) => "assistant")
  (check (pi-assistant-message-stop-reason parsed) => "toolUse")
  (let* ((content (pi-assistant-message-content parsed))
         (text-blk (list-ref content 0))
         (tool-blk (list-ref content 1))
        ) ;
    (check (pi-text-block-text text-blk) => "Hello from ZAI")
    (check (pi-tool-call-block-id tool-blk) => "call_1")
    (check (pi-tool-call-block-name tool-blk) => "bash")
    (check (pi-tool-call-block-arguments tool-blk) => '(("command" . "ls")))
  ) ;let*
) ;let*

(check-report)
