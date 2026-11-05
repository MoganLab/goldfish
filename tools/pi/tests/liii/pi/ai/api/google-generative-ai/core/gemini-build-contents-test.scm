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

(import (liii check)
  (liii pi ai api google-generative-ai core)
  (liii pi ai event)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; user / assistant / toolResult 角色映射
(let* ((msgs (list '((role . "user") (content . "List"))
                   '((role . "assistant")
                     (content . (((type . "toolCall")
                                  (id . "c1")
                                  (name . "ls")
                                  (arguments . (("path" . ".")))))))
                   '((role . "toolResult")
                     (toolCallId . "c1")
                     (toolName . "ls")
                     (isError . #f)
                     (content . (((type . "text") (text . "ok")))))))
       (contents (gemini-build-contents msgs)))
  (check (vector-length contents) => 3)
  (check (cdr (assoc "role" (vector-ref contents 0))) => "user")
  (check (cdr (assoc "role" (vector-ref contents 1))) => "model")
  (check (cdr (assoc "role" (vector-ref contents 2))) => "user")
  (let ((parts (cdr (assoc "parts" (vector-ref contents 1)))))
    (check (vector-length parts) => 1)
    (check (cdr (assoc "name" (cdr (assoc "functionCall" (vector-ref parts 0))))) => "ls")
  ) ;let
) ;let*

;; 字符串键与向量 content 形态的会话恢复消息
(let* ((restored-msgs
        (list '(("role" . "user") ("content" . "hello string key"))
              '(("role" . "assistant") ("content" . #((("type" . "text") ("text" . "hi back")))))
              '(("role" . "toolResult") ("toolCallId" . "call_1") ("toolName" . "bash") ("isError" . #f)
                                        ("content" . #((("type" . "text") ("text" . "result text")))))))
       (contents (gemini-build-contents restored-msgs)))
  (check (vector-length contents) => 3)
  (check (cdr (assoc "role" (vector-ref contents 0))) => "user")
  (check (cdr (assoc "role" (vector-ref contents 1))) => "model")
  (check (cdr (assoc "role" (vector-ref contents 2))) => "user")
) ;let*

;; 空文本块被过滤
(let* ((msgs (list '((role . "user") (content . "hello"))
                   '((role . "assistant")
                     (content . (((type . "text") (text . "   "))
                                 ((type . "toolCall") (id . "c2") (name . "bash") (arguments . ())))))))
       (contents (gemini-build-contents msgs))
       (parts (cdr (assoc "parts" (vector-ref contents 1)))))
  (check (vector-length parts) => 1)
  (check (cdr (assoc "name" (cdr (assoc "functionCall" (vector-ref parts 0))))) => "bash")
) ;let*

;; 空 content 的 assistant 消息被整体跳过
(let* ((msgs (list '((role . "user") (content . "hello"))
                   '((role . "assistant") (content . ()))
                   '((role . "user") (content . "world"))))
       (contents (gemini-build-contents msgs)))
  (check (vector-length contents) => 2)
  (check (cdr (assoc "role" (vector-ref contents 0))) => "user")
  (check (cdr (assoc "role" (vector-ref contents 1))) => "user")
) ;let*

;; 连续 toolResult 合并为一个 user turn
(let* ((msgs (list '((role . "user") (content . "run"))
                   '((role . "toolResult")
                     (toolCallId . "c1")
                     (toolName . "read")
                     (isError . #f)
                     (content . (((type . "text") (text . "one")))))
                   '((role . "toolResult")
                     (toolCallId . "c2")
                     (toolName . "bash")
                     (isError . #f)
                     (content . (((type . "text") (text . "two")))))))
       (contents (gemini-build-contents msgs))
       (merged (vector-ref contents 1))
       (parts (cdr (assoc "parts" merged))))
  (check (vector-length contents) => 2)
  (check (vector-length parts) => 2)
  (check (cdr (assoc "name" (cdr (assoc "functionResponse" (vector-ref parts 0))))) => "read")
  (check (cdr (assoc "name" (cdr (assoc "functionResponse" (vector-ref parts 1))))) => "bash")
) ;let*

;; 空消息列表
(check (vector-length (gemini-build-contents '())) => 0)

;; toolResult 包含图片块时，在 functionResponse 中正确封装 inlineData 结构
(let* ((msgs (list (pi-user-message "read logo.png")
                   (pi-assistant-message
                     (list (pi-tool-call-block "c_img" "read" '(("path" . "logo.png")))))
                   (pi-tool-result-message "c_img" "read"
                     (list (pi-text-block "Read image file [image/png]")
                           (pi-image-block "aGVsbG8=" "image/png"))
                     #f)))
       (contents (gemini-build-contents msgs)))
  (check (vector-length contents) => 3)
  (check (cdr (assoc "role" (vector-ref contents 0))) => "user")
  (check (cdr (assoc "role" (vector-ref contents 1))) => "model")
  (let* ((turn (vector-ref contents 2))
         (parts (cdr (assoc "parts" turn))))
    (check (cdr (assoc "role" turn)) => "user")
    (check (vector-length parts) => 1)
    (let* ((fr (cdr (assoc "functionResponse" (vector-ref parts 0))))
           (fr-parts (cdr (assoc "parts" fr))))
      (check (cdr (assoc "name" fr)) => "read")
      (check (cdr (assoc "id" fr)) => "c_img")
      (check-true (vector? fr-parts))
      (check (vector-length fr-parts) => 1)
      (let ((inline (cdr (assoc "inlineData" (vector-ref fr-parts 0)))))
        (check (cdr (assoc "mimeType" inline)) => "image/png")
        (check (cdr (assoc "data" inline)) => "aGVsbG8=")))))

(check-report)
