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
(let* ((msgs
         (list '((role . "user") (content . "List"))
           '((role . "assistant")
             (content ((type . "toolCall")
                       (id . "c1")
                       (name . "ls")
                       (arguments ("path" . ".")))))
           '((role . "toolResult")
             (toolCallId . "c1")
             (toolName . "ls")
             (isError . #f)
             (content ((type . "text") (text . "ok"))))
         ) ;list
       ) ;msgs
       (contents (gemini-build-contents msgs))
      ) ;
  (check (vector-length contents) => 3)
  (check (cdr (assoc "role" (vector-ref contents 0))) => "user")
  (check (cdr (assoc "role" (vector-ref contents 1))) => "model")
  (check (cdr (assoc "role" (vector-ref contents 2))) => "user")
  (let ((parts (cdr (assoc "parts" (vector-ref contents 1)))))
    (check (vector-length parts) => 1)
    (check
      (cdr
        (assoc "name" (cdr (assoc "functionCall" (vector-ref parts 0))))
      ) ;cdr
      =>
      "ls"
    ) ;check
  ) ;let
) ;let*

;; 字符串键与向量 content 形态的会话恢复消息
(let* ((restored-msgs (list '(("role" . "user")
                              ("content" . "hello string key"))
                        '(("role" . "assistant")
                          ("content"
                           . #((("type" . "text") ("text" . "hi back")))))
                        '(("role" . "toolResult")
                          ("toolCallId" . "call_1")
                          ("toolName" . "bash")
                          ("isError" . #f)
                          ("content"
                           . #((("type" . "text") ("text" . "result text")))))
                      ) ;list
       ) ;restored-msgs
       (contents (gemini-build-contents restored-msgs))
      ) ;
  (check (vector-length contents) => 3)
  (check (cdr (assoc "role" (vector-ref contents 0))) => "user")
  (check (cdr (assoc "role" (vector-ref contents 1))) => "model")
  (check (cdr (assoc "role" (vector-ref contents 2))) => "user")
) ;let*

;; 空文本块被过滤
(let* ((msgs
         (list '((role . "user") (content . "hello"))
           '((role . "assistant")
             (content ((type . "text") (text . "   "))
               ((type . "toolCall") (id . "c2") (name . "bash") (arguments))))
         ) ;list
       ) ;msgs
       (contents (gemini-build-contents msgs))
       (parts (cdr (assoc "parts" (vector-ref contents 1))))
      ) ;
  (check (vector-length parts) => 1)
  (check
    (cdr
      (assoc "name" (cdr (assoc "functionCall" (vector-ref parts 0))))
    ) ;cdr
    =>
    "bash"
  ) ;check
) ;let*

;; 空 content 的 assistant 消息被整体跳过
(let* ((msgs (list '((role . "user") (content . "hello"))
               '((role . "assistant") (content))
               '((role . "user") (content . "world"))
             ) ;list
       ) ;msgs
       (contents (gemini-build-contents msgs))
      ) ;
  (check (vector-length contents) => 2)
  (check (cdr (assoc "role" (vector-ref contents 0))) => "user")
  (check (cdr (assoc "role" (vector-ref contents 1))) => "user")
) ;let*

;; 连续 toolResult 合并为一个 user turn
(let* ((msgs
         (list '((role . "user") (content . "run"))
           '((role . "toolResult")
             (toolCallId . "c1")
             (toolName . "read")
             (isError . #f)
             (content ((type . "text") (text . "one"))))
           '((role . "toolResult")
             (toolCallId . "c2")
             (toolName . "bash")
             (isError . #f)
             (content ((type . "text") (text . "two"))))
         ) ;list
       ) ;msgs
       (contents (gemini-build-contents msgs))
       (merged (vector-ref contents 1))
       (parts (cdr (assoc "parts" merged)))
      ) ;
  (check (vector-length contents) => 2)
  (check (vector-length parts) => 2)
  (check
    (cdr
      (assoc "name" (cdr (assoc "functionResponse" (vector-ref parts 0))))
    ) ;cdr
    =>
    "read"
  ) ;check
  (check
    (cdr
      (assoc "name" (cdr (assoc "functionResponse" (vector-ref parts 1))))
    ) ;cdr
    =>
    "bash"
  ) ;check
) ;let*

;; 空消息列表
(check (vector-length (gemini-build-contents '())) => 0)

;; toolResult 包含图片块时，在 functionResponse 中正确封装 inlineData 结构
(let* ((msgs
         (list (pi-user-message "read logo.png")
           (pi-assistant-message
             (list (pi-tool-call-block "c_img" "read" '(("path" . "logo.png"))))
           ) ;pi-assistant-message
           (pi-tool-result-message "c_img"
             "read"
             (list (pi-text-block "Read image file [image/png]")
               (pi-image-block "aGVsbG8=" "image/png")
             ) ;list
             #f
           ) ;pi-tool-result-message
         ) ;list
       ) ;msgs
       (contents (gemini-build-contents msgs))
      ) ;
  (check (vector-length contents) => 3)
  (check (cdr (assoc "role" (vector-ref contents 0))) => "user")
  (check (cdr (assoc "role" (vector-ref contents 1))) => "model")
  (let* ((turn (vector-ref contents 2)) (parts (cdr (assoc "parts" turn))))
    (check (cdr (assoc "role" turn)) => "user")
    (check (vector-length parts) => 1)
    (let* ((fr (cdr (assoc "functionResponse" (vector-ref parts 0))))
           (fr-parts (cdr (assoc "parts" fr)))
          ) ;
      (check (cdr (assoc "name" fr)) => "read")
      (check (cdr (assoc "id" fr)) => "c_img")
      (check-true (vector? fr-parts))
      (check (vector-length fr-parts) => 1)
      (let ((inline (cdr (assoc "inlineData" (vector-ref fr-parts 0)))))
        (check (cdr (assoc "mimeType" inline)) => "image/png")
        (check (cdr (assoc "data" inline)) => "aGVsbG8=")
      ) ;let
    ) ;let*
  ) ;let*
) ;let*

;; ============ thinking 块与 thought signature 回放 ============

(define (mk-assistant-model blocks . rest)
  (let ((model-id (if (pair? rest) (car rest) "gemini-3-flash-preview")))
    (list (cons "role" "assistant")
      (cons "model" model-id)
      (cons "provider" "google")
      (cons "content" blocks)
      (cons "stopReason" "stop")
    ) ;list
  ) ;let
) ;define

(define (part-of content idx)
  (vector-ref (cdr (assoc "parts" content)) idx)
) ;define

(define (mk-thinking th . sig)
  (let ((base (list (cons "type" "thinking") (cons "thinking" th))))
    (if (pair? sig) (cons (cons "thinkingSignature" (car sig)) base) base)
  ) ;let
) ;define

(define (mk-text t . sig)
  (let ((base (list (cons "type" "text") (cons "text" t))))
    (if (pair? sig) (cons (cons "textSignature" (car sig)) base) base)
  ) ;let
) ;define

(define (mk-tool-call id name . sig)
  (let ((base (list (cons "type" "toolCall")
                (cons "id" id)
                (cons "name" name)
                (cons "arguments" '())
              ) ;list
        ) ;base
       ) ;
    (if (pair? sig) (cons (cons "thoughtSignature" (car sig)) base) base)
  ) ;let
) ;define

;; T1 thinking 块回放为 {thought: true, text, thoughtSignature}
(let* ((msgs
         (list (mk-assistant-model (list (mk-thinking "reasoning..." "QUJD"))))
       ) ;msgs
       (contents (gemini-build-contents msgs "gemini-3-flash-preview"))
       (part (part-of (vector-ref contents 0) 0))
      ) ;
  (check (cdr (assoc "thought" part)) => #t)
  (check (cdr (assoc "text" part)) => "reasoning...")
  (check (cdr (assoc "thoughtSignature" part)) => "QUJD")
) ;let*

;; T2 text 块携带 textSignature 回放
(let* ((msgs
         (list (mk-assistant-model (list (mk-text "visible" "REVG"))))
       ) ;msgs
       (contents (gemini-build-contents msgs "gemini-3-flash-preview"))
       (part (part-of (vector-ref contents 0) 0))
      ) ;
  (check (cdr (assoc "text" part)) => "visible")
  (check (cdr (assoc "thoughtSignature" part)) => "REVG")
) ;let*

;; T3 空文本但带签名的块保留（Gemini 3 要求）
(let* ((msgs
         (list (mk-assistant-model (list (mk-text "" "QUJD"))))
       ) ;msgs
       (contents (gemini-build-contents msgs "gemini-3-flash-preview"))
      ) ;
  (check (vector-length contents) => 1)
  (check
    (cdr (assoc "thoughtSignature" (part-of (vector-ref contents 0) 0)))
    =>
    "QUJD"
  ) ;check
) ;let*

;; T4 空文本且无签名 → 丢弃
(let* ((msgs
         (list (mk-assistant-model (list (mk-text ""))))
       ) ;msgs
       (contents (gemini-build-contents msgs "gemini-3-flash-preview"))
      ) ;
  (check (vector-length contents) => 0)
) ;let*

;; T5 空 thinking 但带签名 → 保留
(let* ((msgs
         (list (mk-assistant-model (list (mk-thinking "" "QUJD"))))
       ) ;msgs
       (contents (gemini-build-contents msgs "gemini-3-flash-preview"))
      ) ;
  (check (vector-length contents) => 1)
  (check
    (cdr (assoc "thought" (part-of (vector-ref contents 0) 0)))
    =>
    #t
  ) ;check
) ;let*

;; T6 跨模型：thinking 块被过滤，不降级为纯文本；正文保留且无签名
(let* ((msgs
         (list (mk-assistant-model (list (mk-thinking "old reasoning" "QUJD") (mk-text "visible reply" "QUJD"))
                 "gemini-2.5-flash"
               ) ;mk-assistant-model
         ) ;list
       ) ;msgs
       (contents (gemini-build-contents msgs "gemini-3-flash-preview"))
       (parts (cdr (assoc "parts" (vector-ref contents 0))))
       (part (vector-ref parts 0))
      ) ;
  (check (vector-length contents) => 1)
  (check (vector-length parts) => 1)
  (check (assoc "thought" part) => #f)
  (check (assoc "thoughtSignature" part) => #f)
  (check (cdr (assoc "text" part)) => "visible reply")
) ;let*

;; T6b 跨模型：仅含 thinking 的 assistant 消息在过滤后为空 turn，被整体跳过
(let* ((msgs
         (list (mk-assistant-model (list (mk-thinking "old reasoning" "QUJD"))
                 "gemini-2.5-flash"
               ) ;mk-assistant-model
         ) ;list
       ) ;msgs
       (contents (gemini-build-contents msgs "gemini-3-flash-preview"))
      ) ;
  (check (vector-length contents) => 0)
) ;let*

;; T7 跨模型：text 块签名不回传
(let* ((msgs
         (list (mk-assistant-model (list (mk-text "hi" "QUJD")) "gemini-2.5-flash"))
       ) ;msgs
       (contents (gemini-build-contents msgs "gemini-3-flash-preview"))
      ) ;
  (check (assoc "thoughtSignature" (part-of (vector-ref contents 0) 0)) => #f)
) ;let*

;; T8 跨模型：原 toolCall 签名不回传，Gemini 3+ 首个 functionCall 补兼容哨兵
(let* ((msgs
         (list (mk-assistant-model (list (mk-tool-call "c1" "read" "QUJD")) "gemini-2.5-flash")
         ) ;list
       ) ;msgs
       (contents (gemini-build-contents msgs "gemini-3-flash-preview"))
      ) ;
  (check (cdr (assoc "thoughtSignature" (part-of (vector-ref contents 0) 0)))
    =>
    "skip_thought_signature_validator"
  ) ;check
) ;let*

;; T9 非 base64 签名不回传（长度非 4 倍数）
(let* ((msgs
         (list (mk-assistant-model (list (mk-thinking "r" "abc"))))
       ) ;msgs
       (contents (gemini-build-contents msgs "gemini-3-flash-preview"))
      ) ;
  (check (assoc "thoughtSignature" (part-of (vector-ref contents 0) 0)) => #f)
) ;let*

;; T10 非 base64 字符的签名不回传
(let* ((msgs
         (list (mk-assistant-model (list (mk-thinking "r" "ab!!"))))
       ) ;msgs
       (contents (gemini-build-contents msgs "gemini-3-flash-preview"))
      ) ;
  (check (assoc "thoughtSignature" (part-of (vector-ref contents 0) 0)) => #f)
) ;let*

;; T11 record 形态的 thinking 块回放
(let* ((msgs
         (list (pi-assistant-message (list (pi-thinking-block "think" "QUJD"))
                 :model    "gemini-3-flash-preview"
                 :provider "google"
               ) ;pi-assistant-message
         ) ;list
       ) ;msgs
       (contents (gemini-build-contents msgs "gemini-3-flash-preview"))
       (part (part-of (vector-ref contents 0) 0))
      ) ;
  (check (cdr (assoc "thought" part)) => #t)
  (check (cdr (assoc "text" part)) => "think")
  (check (cdr (assoc "thoughtSignature" part)) => "QUJD")
) ;let*

;; T12 不带 model-id 参数（旧调用）：保守按跨模型处理，thinking 块丢弃
(let* ((msgs
         (list (mk-assistant-model (list (mk-thinking "r" "QUJD"))))
       ) ;msgs
       (contents (gemini-build-contents msgs))
      ) ;
  (check (vector-length contents) => 0)
) ;let*

(check-report)
