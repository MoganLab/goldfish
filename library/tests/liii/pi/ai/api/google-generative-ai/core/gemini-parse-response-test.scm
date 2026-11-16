;; gemini-parse-response
;; 将 Gemini generateContent JSON 响应解析为内部 assistant message。
;;
;; 语法
;; ----
;; (gemini-parse-response json-data status-code)
;;
;; 参数
;; ----
;; json-data : alist
;; Gemini API 响应对象。
;; status-code : integer
;; HTTP 响应状态码。
;;
;; 返回值
;; ----
;; pi-assistant-message
;; 包含 content、stopReason、usage 和可选 errorMessage 的消息 record。
;;
;; 说明
;; ----
;; text、thinking 和 functionCall 按原始顺序转换为内部 content block。
;; finishReason 映射为 stop、length、toolUse 或 error。

(import (liii check)
  (liii pi ai api google-generative-ai)
 
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; 正常普通回复
(let* ((resp-json '(("candidates"
                     . #((("content" ("parts" . #((("text" . "Hello world")))))
                          ("finishReason" . "STOP"))
                       ) ;#
                    )
                    ("usageMetadata"
                     ("promptTokenCount" . 10)
                     ("candidatesTokenCount" . 5)
                     ("totalTokenCount" . 15)))
       ) ;resp-json
       (parsed (gemini-parse-response resp-json 200))
      ) ;
  (check (pi-assistant-message-role parsed) => "assistant")
  (check (pi-assistant-message-stop-reason parsed) => "stop")
  (let ((content (pi-assistant-message-content parsed)))
    (check (length content) => 1)
    (check (pi-text-block-text (car content)) => "Hello world")
  ) ;let
  (let ((u (pi-assistant-message-usage parsed)))
    (check (cdr (assoc 'input u)) => 10)
    (check (cdr (assoc 'output u)) => 5)
  ) ;let
) ;let*

;; functionCall 回复
(let* ((resp-json '(("candidates"
                     . #((("content"
                           ("parts"
                            . #((("functionCall"
                                  ("name" . "bash")
                                  ("args" ("command" . "ls"))
                                  ("id" . "call_123")))
                              ) ;#
                           ))
                          ("finishReason" . "STOP"))
                       ) ;#
                    ))
       ) ;resp-json
       (parsed (gemini-parse-response resp-json 200))
      ) ;
  (check (pi-assistant-message-stop-reason parsed) => "toolUse")
  (let ((content (pi-assistant-message-content parsed)))
    (check (length content) => 1)
    (let ((blk (car content)))
      (check (pi-block-type blk) => "toolCall")
      (check (pi-tool-call-block-id blk) => "call_123")
      (check (pi-tool-call-block-name blk) => "bash")
      (check
        (cdr (assoc "command" (pi-tool-call-block-arguments blk)))
        =>
        "ls"
      ) ;check
    ) ;let
  ) ;let
) ;let*

;; MAX_TOKENS 截断
(let* ((resp-json '(("candidates"
                     . #((("content" ("parts" . #((("text" . "partial")))))
                          ("finishReason" . "MAX_TOKENS"))
                       ) ;#
                    ))
       ) ;resp-json
       (parsed (gemini-parse-response resp-json 200))
      ) ;
  (check (pi-assistant-message-stop-reason parsed) => "length")
) ;let*

;; SAFETY 报错
(let* ((resp-json '(("candidates"
                     . #((("content" ("parts" . #()))
                          ("finishReason" . "SAFETY"))
                       ) ;#
                    ))
       ) ;resp-json
       (parsed (gemini-parse-response resp-json 200))
      ) ;
  (check (pi-assistant-message-stop-reason parsed) => "error")
  (check (pi-assistant-message-error-message parsed) => "SAFETY")
) ;let*

;; 带有 thinking part 的响应解析
(let* ((resp-json '(("candidates"
                     . #((("content"
                           ("parts"
                            . #((("text" . "Let me ponder...")
                                 ("thought" . #t)
                                 ("thoughtSignature" . "sig-abc"))
                                (("text" . "The final answer."))
                              ) ;#
                           ))
                          ("finishReason" . "STOP"))
                       ) ;#
                    ))
       ) ;resp-json
       (parsed (gemini-parse-response resp-json 200))
      ) ;
  (check (pi-assistant-message-role parsed) => "assistant")
  (check (pi-assistant-message-stop-reason parsed) => "stop")
  (let ((content (pi-assistant-message-content parsed)))
    (check (length content) => 2)
    (let ((b1 (car content)) (b2 (cadr content)))
      (check (pi-block-type b1) => "thinking")
      (check (pi-thinking-block-thinking b1) => "Let me ponder...")
      (check (pi-thinking-block-thinking-signature b1) => "sig-abc")
      (check (pi-block-type b2) => "text")
      (check (pi-text-block-text b2) => "The final answer.")
    ) ;let
  ) ;let
) ;let*

;; functionCall 缺少 id 时自动生成合规 id（防回归）
(let* ((resp-json '(("candidates"
                     . #((("content"
                           ("parts"
                            . #((("functionCall"
                                  ("name" . "bash")
                                  ("args" ("command" . "ls"))))
                              ) ;#
                           ))
                          ("finishReason" . "STOP"))
                       ) ;#
                    ))
       ) ;resp-json
       (parsed (gemini-parse-response resp-json 200))
      ) ;
  (check (pi-assistant-message-stop-reason parsed) => "toolUse")
  (let ((content (pi-assistant-message-content parsed)))
    (check (length content) => 1)
    (let ((blk (car content)))
      (check (pi-block-type blk) => "toolCall")
      (check (pi-tool-call-block-name blk) => "bash")
      (check-true (string? (pi-tool-call-block-id blk)))
      (check-true
        (> (string-length (pi-tool-call-block-id blk)) 0)
      ) ;check-true
    ) ;let
  ) ;let
) ;let*

(check-report)
