;; pi-normalize-transcript
;; 请求前转录规范化：在构建 provider 请求体前统一规范化消息列表，
;; 对齐上游 pi-ai 的 transformMessages.ts。仅作用于请求构建，不改持久化。
;;
;; 语法
;; ----
;; (pi-normalize-transcript messages config)
;;
;; 参数
;; ----
;; messages : list?
;; 内部消息 alist 列表（user/assistant/toolResult），role 为字符串。
;; config : list?
;; 至少包含 (provider-api . "google-generative-ai") 与 (model-id . "...")。
;;
;; 返回值
;; ----
;; list?
;; 规范化后的消息列表：
;; 1. stopReason 为 "error"/"aborted" 的 assistant 消息被移除；
;; 2. 孤儿 toolCall（无对应 toolResult 即被 user/assistant 打断或位于末尾）
;;    补一条合成 toolResult：content 为 "No result provided"，isError 为 #t；
;; 3. Gemini 3+ 模型下，assistant 的 toolCall id 与 toolResult 的 toolCallId
;;    同步归一化（非法字符转 _，超 64 字符截断）。

(import (liii check)
  (liii list)
  (liii pi ai transcript)
) ;import

(check-set-mode! 'report-failed)

(define (msg-role m) (cdr (assoc "role" m)))
(define (msg-content m) (cdr (assoc "content" m)))
(define (msg-stop m)
  (let ((p (assoc "stopReason" m))) (and p (cdr p)))
) ;define
(define (blk-type b) (cdr (assoc "type" b)))
(define (blk-id b) (cdr (assoc "id" b)))
(define (blk-name b) (cdr (assoc "name" b)))

(define (mk-user text)
  (list (cons "role" "user") (cons "content" text))
) ;define
(define (mk-assistant blocks . rest)
  (let ((stop (if (pair? rest) (car rest) "stop")))
    (list (cons "role" "assistant")
      (cons "content" blocks)
      (cons "stopReason" stop)
    ) ;list
  ) ;let
) ;define
(define (mk-text-blk text)
  (list (cons "type" "text") (cons "text" text))
) ;define
(define (mk-tool-call id name)
  (list (cons "type" "toolCall") (cons "id" id) (cons "name" name) (cons "arguments" '()))
) ;define
(define (mk-tool-result id name text . rest)
  (let ((is-err (if (pair? rest) (car rest) #f)))
    (list (cons "role" "toolResult")
      (cons "toolCallId" id)
      (cons "toolName" name)
      (cons "content" (list (mk-text-blk text)))
      (cons "isError" is-err)
    ) ;list
  ) ;let
) ;define

(define gemini-cfg '((provider-api . "google-generative-ai") (model-id . "gemini-3-flash-preview")))
(define openai-cfg '((provider-api . "openai-completions") (model-id . "gpt-5.2")))

(define (tool-result-text m)
  (let ((p (assoc "text" (car (msg-content m))))) (and p (cdr p)))
) ;define

;; ============ A：孤儿 toolCall 补合成 toolResult ============

;; A1 assistant 含 toolCall，后随 user（无结果）→ 之间插入合成 toolResult
(let* ((msgs (list (mk-user "hi")
               (mk-assistant (list (mk-tool-call "c1" "read")) "toolUse")
               (mk-user "next")
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
      ) ;
  (check (length out) => 4)
  (check (msg-role (list-ref out 2)) => "toolResult")
  (check (cdr (assoc "toolCallId" (list-ref out 2))) => "c1")
  (check (cdr (assoc "toolName" (list-ref out 2))) => "read")
  (check (tool-result-text (list-ref out 2)) => "No result provided")
  (check (cdr (assoc "isError" (list-ref out 2))) => #t)
  (check (msg-role (list-ref out 3)) => "user")
) ;let*

;; A2 两个 toolCall 仅一个有结果 → 只补缺失的那个
(let* ((msgs (list (mk-assistant (list (mk-tool-call "c1" "read") (mk-tool-call "c2" "bash"))
                   "toolUse"
               ) ;mk-assistant
               (mk-tool-result "c1" "read" "ok")
               (mk-user "next")
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
      ) ;
  (check (length out) => 4)
  (check (msg-role (list-ref out 1)) => "toolResult")
  (check (cdr (assoc "toolCallId" (list-ref out 1))) => "c1")
  (check (msg-role (list-ref out 2)) => "toolResult")
  (check (cdr (assoc "toolCallId" (list-ref out 2))) => "c2")
  (check (tool-result-text (list-ref out 2)) => "No result provided")
  (check (msg-role (list-ref out 3)) => "user")
) ;let*

;; A3 所有 toolCall 均有结果 → 原样不变
(let* ((msgs (list (mk-assistant (list (mk-tool-call "c1" "read")) "toolUse")
               (mk-tool-result "c1" "read" "ok")
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
      ) ;
  (check (length out) => 2)
  (check out => msgs)
) ;let*

;; A4 列表以未回应 toolCall 结尾 → 末尾补齐
(let* ((msgs (list (mk-user "hi") (mk-assistant (list (mk-tool-call "c9" "edit")) "toolUse")))
       (out (pi-normalize-transcript msgs gemini-cfg))
      ) ;
  (check (length out) => 3)
  (check (msg-role (list-ref out 2)) => "toolResult")
  (check (cdr (assoc "toolCallId" (list-ref out 2))) => "c9")
) ;let*

;; A5 孤儿 toolCall 后接另一条 assistant（非 user）→ 同样补全
(let* ((msgs (list (mk-assistant (list (mk-tool-call "c1" "read")) "toolUse")
               (mk-assistant (list (mk-text-blk "partial")) "stop")
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
      ) ;
  (check (length out) => 3)
  (check (msg-role (list-ref out 1)) => "toolResult")
  (check (cdr (assoc "toolCallId" (list-ref out 1))) => "c1")
) ;let*

;; ============ B：error/aborted assistant 消息跳过 ============

;; B1 stopReason "error" 的 assistant 被移除
(let* ((msgs (list (mk-user "hi")
               (mk-assistant (list (mk-text-blk "")) "error")
               (mk-user "again")
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
      ) ;
  (check (length out) => 2)
  (check (map msg-role out) => '("user" "user"))
) ;let*

;; B2 stopReason "aborted" 的 assistant 被移除
(let* ((msgs (list (mk-assistant (list (mk-text-blk "partial")) "aborted")
               (mk-user "next")
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
      ) ;
  (check (map msg-role out) => '("user"))
) ;let*

;; B3 正常 stop/toolUse 消息不受影响
(let* ((msgs (list (mk-assistant (list (mk-text-blk "hello")) "stop")
               (mk-assistant (list (mk-tool-call "c1" "read")) "toolUse")
               (mk-tool-result "c1" "read" "ok")
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
      ) ;
  (check (length out) => 3)
  (check out => msgs)
) ;let*

;; ============ C：toolCallId 归一化（仅 Gemini 3+） ============

;; C1 非法字符 → _，toolResult 联动
(let* ((msgs (list (mk-assistant (list (mk-tool-call "call|abc:1" "read")) "toolUse")
               (mk-tool-result "call|abc:1" "read" "ok")
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
       (a-blk (car (msg-content (list-ref out 0))))
       (r-msg (list-ref out 1))
      ) ;
  (check (blk-id a-blk) => "call_abc_1")
  (check (cdr (assoc "toolCallId" r-msg)) => "call_abc_1")
  (check (blk-name a-blk) => "read")
) ;let*

;; C2 超 64 字符 → 截断，toolResult 联动
(let* ((long-id (make-string 70 #\a))
       (msgs (list (mk-assistant (list (mk-tool-call long-id "read")) "toolUse")
               (mk-tool-result long-id "read" "ok")
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
      ) ;
  (check (string-length (blk-id (car (msg-content (car out))))) => 64)
  (check (cdr (assoc "toolCallId" (cadr out))) => (make-string 64 #\a))
) ;let*

;; C3 合法 id → 不变
(let* ((msgs (list (mk-assistant (list (mk-tool-call "call_OK-123" "read")) "toolUse")
               (mk-tool-result "call_OK-123" "read" "ok")
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
      ) ;
  (check (blk-id (car (msg-content (car out)))) => "call_OK-123")
  (check (cdr (assoc "toolCallId" (cadr out))) => "call_OK-123")
) ;let*

;; C4 归一化后碰撞：后者覆盖（沿用上游 map 覆盖语义）
(let* ((msgs (list (mk-assistant (list (mk-tool-call "a|b" "read") (mk-tool-call "a:b" "bash"))
                   "toolUse"
               ) ;mk-assistant
               (mk-tool-result "a|b" "read" "r1")
               (mk-tool-result "a:b" "bash" "r2")
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
      ) ;
  ;; 两个 id 均归一化为 a_b；映射以后注册的为准，此处只要求结果可构造且不报错
  (check (string? (blk-id (car (msg-content (car out))))) => #t)
  (check (length out) => 3)
) ;let*

;; C5 非 Gemini 3+（openai）→ 不归一化
(let* ((msgs (list (mk-assistant (list (mk-tool-call "call|abc" "read")) "toolUse")
               (mk-tool-result "call|abc" "read" "ok")
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs openai-cfg))
      ) ;
  (check (blk-id (car (msg-content (car out)))) => "call|abc")
  (check (cdr (assoc "toolCallId" (cadr out))) => "call|abc")
) ;let*

;; C6 Gemini 2.5（major < 3）→ 不归一化
(let* ((cfg '((provider-api . "google-generative-ai") (model-id . "gemini-2.5-flash")))
       (msgs (list (mk-assistant (list (mk-tool-call "call|abc" "read")) "toolUse")
               (mk-tool-result "call|abc" "read" "ok")
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs cfg))
      ) ;
  (check (blk-id (car (msg-content (car out)))) => "call|abc")
) ;let*

;; C7 合成 toolResult 使用归一化后的 id（孤儿 + 非法字符）
(let* ((msgs (list (mk-assistant (list (mk-tool-call "c|1" "read")) "toolUse") (mk-user "next")))
       (out (pi-normalize-transcript msgs gemini-cfg))
      ) ;
  (check (blk-id (car (msg-content (list-ref out 0)))) => "c_1")
  (check (cdr (assoc "toolCallId" (list-ref out 1))) => "c_1")
) ;let*

;; ============ D：边界 ============

;; D1 空列表
(check (pi-normalize-transcript '() gemini-cfg) => '())

;; D2 仅 user 消息
(let ((msgs (list (mk-user "hi"))))
  (check (pi-normalize-transcript msgs gemini-cfg) => msgs)
) ;let

;; D3 非 assistant 消息上的 stopReason 不影响 user/toolResult
(let* ((msgs (list (mk-user "hi") (mk-tool-result "c1" "read" "ok")))
       (out (pi-normalize-transcript msgs gemini-cfg))
      ) ;
  (check (length out) => 2)
) ;let*

(check-report)
