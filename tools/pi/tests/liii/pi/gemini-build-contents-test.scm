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

;; 过滤空文本块：assistant 包含空文本块与 toolCall 时，空文本块不应出现在 parts 中
(let* ((msgs (list '((role . "user") (content . "🚀 <"))
                   '((role . "assistant") (content . (((type . "toolCall") (id . "call_1") (name . "bash") (arguments . (("command" . "ls"))))
                                                     ((type . "text") (text . ""))
                                                     ((type . "text") (text . "   ")))))
                   '((role . "toolResult") (toolCallId . "call_1") (toolName . "bash") (isError . #f) (content . (((type . "text") (text . "ok")))))))
       (contents (gemini-build-contents msgs))
       (model-turn (vector-ref contents 1))
       (model-parts (cdr (assoc "parts" model-turn))))
  (check (vector-length contents) => 3)
  (check (vector-length model-parts) => 1)
  (check (cdr (assoc "name" (cdr (assoc "functionCall" (vector-ref model-parts 0))))) => "bash"))

;; 支持 record 结构（强类型消息与 content block）
(let* ((msgs (list (pi-user-message "List files")
                   (pi-assistant-message
                     (list (pi-tool-call-block "c1" "ls" '(("path" . ".")))
                           (pi-text-block "Checking directory")))
                   (pi-tool-result-message "c1" "ls" (list (pi-text-block "file.txt")) #f)))
       (contents (gemini-build-contents msgs)))
  (check (vector-length contents) => 3)
  (check (cdr (assoc "role" (vector-ref contents 0))) => "user")
  (check (cdr (assoc "role" (vector-ref contents 1))) => "model")
  (check (cdr (assoc "role" (vector-ref contents 2))) => "user"))

(check-report)
