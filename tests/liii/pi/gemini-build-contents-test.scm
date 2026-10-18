(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; gemini-build-contents
;; 将消息历史转换为 Gemini 接口 contents 向量。
(let* ((msgs (list '((role . "user") (content . "List"))
                   '((role . "assistant") (content . (((type . "toolCall") (id . "c1") (name . "ls") (arguments . (("path" . ".")))))))
                   '((role . "toolResult") (toolCallId . "c1") (toolName . "ls") (isError . #f) (content . (((type . "text") (text . "ok")))))))
       (contents (gemini-build-contents msgs)))
  (check (vector-length contents) => 3)
  (check (cdr (assoc "role" (vector-ref contents 0))) => "user")
  (check (cdr (assoc "role" (vector-ref contents 1))) => "model")
  (check (cdr (assoc "role" (vector-ref contents 2))) => "user"))

;; 支持从 session 恢复的消息结构（字符串键与向量/block content）
(let* ((restored-msgs
        (list '(("role" . "user") ("content" . "hello string key"))
              '(("role" . "assistant") ("content" . #((("type" . "text") ("text" . "hi back")))))
              '(("role" . "toolResult") ("toolCallId" . "call_1") ("toolName" . "bash") ("isError" . #f)
                                        ("content" . #((("type" . "text") ("text" . "result text")))))))
       (contents (gemini-build-contents restored-msgs)))
  (check (vector-length contents) => 3)
  (check (cdr (assoc "role" (vector-ref contents 0))) => "user")
  (check (cdr (assoc "role" (vector-ref contents 1))) => "model")
  (check (cdr (assoc "role" (vector-ref contents 2))) => "user"))

(check-report)
