(import (liii check)
  (liii pi ai api google-generative-ai)
  (liii pi ai event)
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
  (check (cdr (assoc 'role parsed)) => "assistant")
  (check (cdr (assoc 'stopReason parsed)) => "stop")
  (let ((content (cdr (assoc 'content parsed))))
    (check (length content) => 1)
    (check (cdr (assoc 'text (car content))) => "Hello world")
  ) ;let
  (let ((u (cdr (assoc 'usage parsed))))
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
  (check (cdr (assoc 'stopReason parsed)) => "toolUse")
  (let ((content (cdr (assoc 'content parsed))))
    (check (length content) => 1)
    (let ((blk (car content)))
      (check (cdr (assoc 'type blk)) => "toolCall")
      (check (cdr (assoc 'id blk)) => "call_123")
      (check (cdr (assoc 'name blk)) => "bash")
      (check
        (cdr (assoc "command" (cdr (assoc 'arguments blk))))
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
  (check (cdr (assoc 'stopReason parsed)) => "length")
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
  (check (cdr (assoc 'stopReason parsed)) => "error")
  (check (cdr (assoc 'errorMessage parsed)) => "SAFETY")
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
  (check (cdr (assoc 'role parsed)) => "assistant")
  (check (cdr (assoc 'stopReason parsed)) => "stop")
  (let ((content (cdr (assoc 'content parsed))))
    (check (length content) => 2)
    (let ((b1 (car content)) (b2 (cadr content)))
      (check (cdr (assoc 'type b1)) => "thinking")
      (check (cdr (assoc 'thinking b1)) => "Let me ponder...")
      (check (cdr (assoc 'thinkingSignature b1)) => "sig-abc")
      (check (cdr (assoc 'type b2)) => "text")
      (check (cdr (assoc 'text b2)) => "The final answer.")
    ) ;let
  ) ;let
) ;let*

(check-report)
