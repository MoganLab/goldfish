(import (liii check) (liii pi ai event) (liii pi ai api google-generative-ai) (liii pi ai message))

(check-set-mode! 'report-failed)

;; gemini-build-contents
;; 将内部消息历史转换为 Gemini API contents。
;;
;; 语法
;; ----
;; (gemini-build-contents messages)
;;
;; 参数
;; ----
;; messages : list
;; 内部 user、assistant 和 toolResult 消息列表，支持 alist 和 record。
;;
;; 返回值
;; ----
;; vector
;; Gemini contents 向量，每个元素包含 role 和 parts。
;;
;; 说明
;; ----
;; assistant 映射为 model，空文本块被过滤；整条空 content 的 assistant 消息
;; （如 ESC abort 后 stopReason=aborted 的残留）被整体跳过，
;; 跳过后允许产生连续 user 消息。
;; 连续 toolResult 合并为一个 user turn，parts 顺序保持不变。
;;
;; 示例
;; ----
;; (gemini-build-contents (list (pi-user-message "hello"))) => #((("role" . "user") ...))
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

;; 连续 toolResult 合并为一个 user turn，且 parts 顺序不变
(let* ((msgs (list (pi-user-message "run")
                   (pi-tool-result-message "c1" "read" (list (pi-text-block "one")) #f)
                   (pi-tool-result-message "c2" "bash" (list (pi-text-block "two")) #f)))
       (contents (gemini-build-contents msgs))
       (merged (vector-ref contents 1))
       (parts (cdr (assoc "parts" merged))))
  (check (vector-length contents) => 2)
  (check (vector-length parts) => 2)
  (check (cdr (assoc "id" (cdr (assoc "functionResponse" (vector-ref parts 0))))) => "c1")
  (check (cdr (assoc "id" (cdr (assoc "functionResponse" (vector-ref parts 1))))) => "c2"))

;; ESC abort 后整条空 content 的 assistant 消息（stopReason=aborted）同样不得进入请求：
;; parts 为空时整条跳过，允许产生连续 user 消息（防回归用例，当前实现已正确）
(let* ((msgs (list (pi-user-message "hello")
                   (pi-assistant-message '() :stop-reason "aborted")
                   (pi-user-message "world")))
       (contents (gemini-build-contents msgs)))
  (check (vector-length contents) => 2)
  (check (cdr (assoc "role" (vector-ref contents 0))) => "user")
  (check (cdr (assoc "role" (vector-ref contents 1))) => "user"))

(check-report)
