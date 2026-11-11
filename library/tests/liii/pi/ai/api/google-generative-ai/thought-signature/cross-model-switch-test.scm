;; cross-model-switch
;; 验证 Gemini 协议层在跨模型切换时对异构思考签名的净化与思维链阻断。
;;
;; 语法
;; ----
;; (gemini-build-contents messages (model-id #f) (provider "google"))
;; (gemini-assistant-block->part blk same-model?)
;;
;; 参数
;; ----
;; messages : list
;; 内部消息历史（支持 alist 与 record），包含跨模型切换历史。
;; model-id : string | #f
;; 当前请求的目标模型标识。
;; provider : string
;; 当前请求的目标服务商标识，默认为 "google"。
;;
;; 返回值
;; ----
;; vector
;; 适用于 Gemini API 的 contents 向量：跨模型时异构思维链被丢弃，
;; 工具调用与文本块上的异构签名被剥离，杜绝 HTTP 400 校验错误。
;;
;; 说明
;; ----
;; 当 Coding Agent 从 MiniMax / Anthropic 等模型切换到 Gemini 时，
;; 历史异构签名若回传会导致 HTTP 400: Corrupted thought signature。

(import (liii check)
  (liii list)
  (liii json)
  (liii pi ai message)
  (liii pi ai api google-generative-ai core)
  (liii pi ai api google-generative-ai thought-signature)
) ;import

(check-set-mode! 'report-failed)

;; 递归检测任意嵌套数据结构中是否存在 thoughtSignature 或类似签名字段

(define (has-thought-signature? obj)
  (cond
   ((pair? obj)
    (or (and (pair? (car obj)) (has-thought-signature? (car obj)))
      (and (or (equal? (car obj) "thoughtSignature")
             (equal? (car obj) 'thoughtSignature)
             (equal? (car obj) "thinkingSignature")
             (equal? (car obj) "textSignature")
           ) ;or
        #t
      ) ;and
      (has-thought-signature? (cdr obj))
    ) ;or
   ) ;
   ((vector? obj)
    (let loop
      ((i 0))
      (cond ((>= i (vector-length obj)) #f)
            ((has-thought-signature? (vector-ref obj i)) #t)
            (else (loop (+ i 1)))
      ) ;cond
    ) ;let
   ) ;
   (else #f)
  ) ;cond
) ;define

;; 递归检测数据结构中是否存在指定的原始签名值

(define (has-raw-signature? obj sig)
  (cond
   ((pair? obj)
    (or (and (pair? (car obj)) (has-raw-signature? (car obj) sig))
      (and (equal? (cdr obj) sig) #t)
      (has-raw-signature? (cdr obj) sig)
    ) ;or
   ) ;
   ((vector? obj)
    (let loop
      ((i 0))
      (cond ((>= i (vector-length obj)) #f)
            ((has-raw-signature? (vector-ref obj i) sig) #t)
            (else (loop (+ i 1)))
      ) ;cond
    ) ;let
   ) ;
   (else #f)
  ) ;cond
) ;define

;; 取 Gemini content 的第 idx 个 part

(define (part-of content idx)
  (vector-ref (cdr (assoc "parts" content)) idx)
) ;define

;; 紧凑块构造：thinking / toolCall / text

(define (mk-th t . sig)
  (if (pair? sig)
    (list (cons "type" "thinking")
      (cons "thinking" t)
      (cons "thinkingSignature" (car sig))
    ) ;list
    (list (cons "type" "thinking") (cons "thinking" t))
  ) ;if
) ;define

(define (mk-tc id name . sig)
  (if (pair? sig)
    (list (cons "type" "toolCall")
      (cons "id" id)
      (cons "name" name)
      (cons "arguments" '())
      (cons "thoughtSignature" (car sig))
    ) ;list
    (list (cons "type" "toolCall")
      (cons "id" id)
      (cons "name" name)
      (cons "arguments" '())
    ) ;list
  ) ;if
) ;define

;; 递归检测数据结构中是否存在 thought 块标记

(define (has-thought-block? obj)
  (cond
   ((pair? obj)
    (or
      (and (or (equal? (car obj) "thought") (equal? (car obj) 'thought)) #t)
      (and (pair? (car obj)) (has-thought-block? (car obj)))
      (has-thought-block? (cdr obj))
    ) ;or
   ) ;
   ((vector? obj)
    (let loop
      ((i 0))
      (cond ((>= i (vector-length obj)) #f)
            ((has-thought-block? (vector-ref obj i)) #t)
            (else (loop (+ i 1)))
      ) ;cond
    ) ;let
   ) ;
   (else #f)
  ) ;cond
) ;define

(define (mk-assistant prov model blocks)
  (let ((base (list (cons "role" "assistant")
                (cons "content" blocks)
                (cons "stopReason" "stop")
              ) ;list
        ) ;base
       ) ;
    (append base
      (if prov (list (cons "provider" prov)) '())
      (if model (list (cons "model" model)) '())
    ) ;append
  ) ;let
) ;define

;; 1. 块级转换：跨模型阻断
;; 1.1 跨模型 thinking 丢弃（返回 #f）
(check (gemini-assistant-block->part '(("type" . "thinking")
                                       ("thinking" . "minimax reasoning")
                                       ("thinkingSignature" . "QUJD"))
         #f
       ) ;gemini-assistant-block->part
  =>
  #f
) ;check
(check (gemini-assistant-block->part '(("type" . "thinking")
                                       ("thinking" . "thought"))
         #f
       ) ;gemini-assistant-block->part
  =>
  #f
) ;check

;; 1.2 跨模型 toolCall 剥离 thoughtSignature
(let* ((blk '(("type" . "toolCall")
              ("id" . "c1")
              ("name" . "bash")
              ("arguments" ("command" . "ls"))
              ("thoughtSignature" . "QUJD"))
       ) ;blk
       (part (gemini-assistant-block->part blk #f))
      ) ;
  (check-true (pair? part))
  (check (assoc "thoughtSignature" part) => #f)
  (check
    (cdr (assoc "name" (cdr (assoc "functionCall" part))))
    =>
    "bash"
  ) ;check
) ;let*

;; 1.3 跨模型 text 块保留但剥离 textSignature
(let* ((blk '(("type" . "text") ("text" . "visible") ("textSignature" . "QUJD")))
       (part (gemini-assistant-block->part blk #f))
      ) ;
  (check (cdr (assoc "text" part)) => "visible")
  (check (assoc "thoughtSignature" part) => #f)
) ;let*

;; 1.4 同模型合法 base64 回放，非法不回放
(let* ((p-ok (gemini-assistant-block->part '(("type" . "thinking")
                                             ("thinking" . "th")
                                             ("thinkingSignature" . "QUJD"))
               #t
             ) ;gemini-assistant-block->part
       ) ;p-ok
       (p-bad (gemini-assistant-block->part '(("type" . "thinking")
                                              ("thinking" . "th")
                                              ("thinkingSignature" . "bad!"))
                #t
              ) ;gemini-assistant-block->part
       ) ;p-bad
      ) ;
  (check (cdr (assoc "thought" p-ok)) => #t)
  (check (cdr (assoc "thoughtSignature" p-ok)) => "QUJD")
  (check (cdr (assoc "thought" p-bad)) => #t)
  (check (assoc "thoughtSignature" p-bad) => #f)
) ;let*

;; 2. MiniMax -> Gemini 切换
(let* ((mm-msg (mk-assistant "minimax"
                 "MiniMax-M2.7"
                 (list (mk-th "MiniMax thought" "bWluaW1heC1zaWc=")
                   (mk-tc "c_mm" "bash" "bWluaW1heC1zaWc=")
                 ) ;list
               ) ;mk-assistant
       ) ;mm-msg
       (msgs (list (list (cons "role" "user") (cons "content" "run")) mm-msg))
       (contents (gemini-build-contents msgs "gemini-2.5-flash" "google"))
      ) ;
  (check (vector-length contents) => 2)
  (check-false (has-thought-signature? contents))
  (check-false (has-thought-block? contents))
  (let* ((parts (cdr (assoc "parts" (vector-ref contents 1))))
         (fc (cdr (assoc "functionCall" (vector-ref parts 0))))
        ) ;
    (check (vector-length parts) => 1)
    (check (cdr (assoc "name" fc)) => "bash")
    (check (cdr (assoc "id" fc)) => "c_mm")
  ) ;let*
) ;let*

;; 3. Anthropic / DeepSeek -> Gemini 切换
(let* ((claude-msg
         (mk-assistant "anthropic"
           "claude-3-7-sonnet"
           (list (list (cons "type" "thinking")
                   (cons "thinking" "Claude thought")
                   (cons "thinkingSignature" "c2ln")
                 ) ;list
             (list (cons "type" "text")
               (cons "text" "Claude reply")
               (cons "textSignature" "c2ln")
             ) ;list
           ) ;list
         ) ;mk-assistant
       ) ;claude-msg
       (msgs (list (list (cons "role" "user") (cons "content" "hi")) claude-msg))
       (contents (gemini-build-contents msgs "gemini-3-flash-preview" "google"))
      ) ;
  (check (vector-length contents) => 2)
  (check-false (has-thought-signature? contents))
  (check-false (has-thought-block? contents))
  (check
    (cdr
      (assoc "text" (vector-ref (cdr (assoc "parts" (vector-ref contents 1))) 0))
    ) ;cdr
    =>
    "Claude reply"
  ) ;check
) ;let*

;; 3.2 纯思考消息过滤后整条跳过
(let ((deepseek-msg
        (mk-assistant "deepseek"
          "deepseek-reasoner"
          (list (list (cons "type" "thinking")
                  (cons "thinking" "R1 thought")
                  (cons "thinkingSignature" "c2ln")
                ) ;list
          ) ;list
        ) ;mk-assistant
      ) ;deepseek-msg
     ) ;
  (check (vector-length (gemini-build-contents (list deepseek-msg) "gemini-2.5-flash" "google")
         ) ;vector-length
    =>
    0
  ) ;check
) ;let

;; 4. 同 Provider 跨模型（2.5 -> 3）：异构签名不回传，
;;    Gemini 3+ 目标下无签名 toolCall 补哨兵
(let* ((g25-msg (mk-assistant "google"
                  "gemini-2.5-flash"
                  (list (mk-th "2.5 thought" "c2ln") (mk-tc "cg" "read" "c2ln"))
                ) ;mk-assistant
       ) ;g25-msg
       (contents (gemini-build-contents (list g25-msg) "gemini-3-flash-preview" "google")
       ) ;contents
      ) ;
  (check (vector-length contents) => 1)
  (check
    (cdr (assoc "thoughtSignature" (part-of (vector-ref contents 0) 0)))
    =>
    "skip_thought_signature_validator"
  ) ;check
  (check-false (has-thought-block? contents))
  (check-false (has-raw-signature? contents "c2ln"))
) ;let*

;; 5. 缺省元数据安全兜底
(let* ((no-m-msg (mk-assistant "google" #f (list (mk-th "th" "QUJD") (mk-tc "c1" "ls" "QUJD")))
       ) ;no-m-msg
       (no-p-msg (mk-assistant #f "gemini-2.5-flash" (list (mk-tc "c2" "ls" "QUJD"))))
       (c1 (gemini-build-contents (list no-m-msg) "gemini-2.5-flash" "google"))
       (c2 (gemini-build-contents (list no-p-msg) "gemini-2.5-flash" "google"))
       (c3 (gemini-build-contents (list no-m-msg)))
      ) ;
  (for-each (lambda (cc)
              (check-false (has-thought-signature? cc))
              (check-false (has-thought-block? cc))
            ) ;lambda
    (list c1 c2 c3)
  ) ;for-each
) ;let*

;; 6. Record 结构体形态（pi-assistant-message）验证
(let* ((mm-rec (pi-assistant-message (list (pi-thinking-block "mm thought" "QUJD")
                                       (pi-tool-call-block "c_rec" "ls" '() "QUJD")
                                     ) ;list
                 :provider "minimax"
                 :model    "MiniMax-M2.7"
               ) ;pi-assistant-message
       ) ;mm-rec
       (gem-rec (pi-assistant-message (list (pi-thinking-block "gem thought" "QUJD")
                                        (pi-tool-call-block "c_gem" "read" '() "QUJD")
                                      ) ;list
                  :provider "google"
                  :model    "gemini-3-flash-preview"
                ) ;pi-assistant-message
       ) ;gem-rec
       (c-cross (gemini-build-contents (list mm-rec) "gemini-2.5-flash" "google"))
       (c-same (gemini-build-contents (list gem-rec) "gemini-3-flash-preview" "google")
       ) ;c-same
      ) ;
  (check-false (has-thought-signature? c-cross))
  (check-false (has-thought-block? c-cross))
  (check-true (has-thought-signature? c-same))
  (check-true (has-thought-block? c-same))
) ;let*

;; 7. 完整请求体构造（gemini-build-request-body）端到端断言
(let* ((msgs
         (list (list (cons "role" "user") (cons "content" "fix code"))
           (mk-assistant "minimax"
             "MiniMax-M2.7"
             (list (mk-th "analyzing..." "bWluaW1heC1zaWc=")
               (list (cons "type" "toolCall")
                 (cons "id" "c_read")
                 (cons "name" "read")
                 (cons "arguments" '(("path" . "a.scm")))
                 (cons "thoughtSignature" "bWluaW1heC1zaWc=")
               ) ;list
             ) ;list
           ) ;mk-assistant
           (list (cons "role" "toolResult")
             (cons "toolCallId" "c_read")
             (cons "toolName" "read")
             (cons "isError" #f)
             (cons "content" (list (list (cons "type" "text") (cons "text" "code"))))
           ) ;list
           (list (cons "role" "user") (cons "content" "next"))
         ) ;list
       ) ;msgs
       (contents (gemini-build-contents msgs "gemini-2.5-flash" "google"))
       (tools
         (list
           (list (cons "name" "read")
             (cons "description" "read file")
             (cons "parameters" '(("type" . "object")))
           ) ;list
         ) ;list
       ) ;tools
       (req-body (gemini-build-request-body contents "You are a coding agent" tools))
       (req-json (json->string req-body))
      ) ;
  (check-true (string? req-json))
  (check-false (has-thought-signature? req-body))
  (check-false (has-thought-block? req-body))
) ;let*

(check-report)
