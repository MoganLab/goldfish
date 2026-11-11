;; chain-rewind-signature
;; 复现并回归 all-in-one 链式调度回溯场景下的签名缺失 400：
;; gemini（带签名 toolCall）→ glm/minimax（无签名 toolCall）→ 回溯 gemini。
;;
;; 背景（2026-11-11 实际故障）：
;;   [SWITCH] google→zai-coding-cn/glm-5.3（gemini 429 后切换，glm 产生 toolUse）
;;   [REWIND] 回 google/gemini-3.8-flash（thinking=medium）
;;   → HTTP 400 "Function call is missing a thought_signature in
;;     functionCall parts"（报错指向 glm 的无签名 bash toolCall）
;;
;; 修复：外来无签名 toolCall 在 Gemini 3+ 目标下补官方哨兵
;; skip_thought_signature_validator（对齐上游 pi badlogic/pi-mono
;; a0d839ce8 的 SKIP_THOUGHT_SIGNATURE 方案）。
;;
;; 语法
;; ----
;; (gemini-build-contents messages model-id provider)
;;
;; 参数
;; ----
;; messages : list?
;; 消息历史（支持 alist 与 record），含跨模型切换与回溯。
;; model-id : string?
;; 当前请求的目标模型标识。
;; provider : string?
;; 当前请求的目标服务商标识。
;;
;; 返回值
;; ----
;; vector
;; contents 向量：同模型 toolCall 保留原签名；外来 toolCall 补哨兵；
;; 异构签名与思维链不回传。

(import (liii check)
  (liii pi ai message)
  (liii pi ai api google-generative-ai core)
) ;import

(check-set-mode! 'report-failed)

(define (part-of content idx)
  (vector-ref (cdr (assoc "parts" content)) idx)
) ;define

;; 构造 2026-11-11 dev-agent 会话的真实序列：
;; U → A(gemini, 带签名) → T → A(gemini, 带签名) → T → A(glm, 无签名) → T

(define user-msg (list (cons "role" "user") (cons "content" "排查问题")))

(define gemini-msg-1
  (list (cons "role" "assistant")
    (cons "model" "gemini-3.8-flash")
    (cons "provider" "google")
    (cons "content"
      (list (list (cons "type" "thinking") (cons "thinking" "gm think"))
        (list (cons "type" "toolCall")
          (cons "id" "tc-1")
          (cons "name" "bash")
          (cons "arguments" (list (cons "command" "git status")))
          (cons "thoughtSignature" "QUJD")
        ) ;list
      ) ;list
    ) ;cons
    (cons "stopReason" "toolUse")
  ) ;list
) ;define

(define gemini-msg-2
  (list (cons "role" "assistant")
    (cons "model" "gemini-3.8-flash")
    (cons "provider" "google")
    (cons "content"
      (list
        (list (cons "type" "toolCall")
          (cons "id" "tc-2")
          (cons "name" "read")
          (cons "arguments" (list (cons "path" "a.scm")))
          (cons "thoughtSignature" "REVG")
        ) ;list
      ) ;list
    ) ;cons
    (cons "stopReason" "toolUse")
  ) ;list
) ;define

;; glm 产生的 toolCall：无 thoughtSignature（这正是故障点）

(define glm-msg
  (list (cons "role" "assistant")
    (cons "model" "glm-5.3")
    (cons "provider" "zai-coding-cn")
    (cons "content"
      (list (list (cons "type" "text") (cons "text" "文件已存在。"))
        (list (cons "type" "toolCall")
          (cons "id" "call_a790c963")
          (cons "name" "bash")
          (cons "arguments" (list (cons "command" "cat a.scm")))
        ) ;list
      ) ;list
    ) ;cons
    (cons "stopReason" "toolUse")
  ) ;list
) ;define

(define (mk-tool-result id name)
  (list (cons "role" "toolResult")
    (cons "toolCallId" id)
    (cons "toolName" name)
    (cons "isError" #f)
    (cons "content" (list (list (cons "type" "text") (cons "text" "ok"))))
  ) ;list
) ;define

;; 回溯后的完整历史：回放给 gemini-3.8-flash（原 400 请求）

(define msgs
  (list user-msg
    gemini-msg-1
    (mk-tool-result "tc-1" "bash")
    gemini-msg-2
    (mk-tool-result "tc-2" "read")
    glm-msg
    (mk-tool-result "call_a790c963" "bash")
  ) ;list
) ;define

;; T1 回溯回 gemini-3.8-flash：每个 functionCall 都带 thoughtSignature
;;    （同模型保留原签名，外来补哨兵），不再触发 400
(let ((contents (gemini-build-contents msgs "gemini-3.8-flash" "google")))
  (check (vector-length contents) => 7)
  ;; turn 1: model, 两个 part（thinking + toolCall）
  (let ((p1 (part-of (vector-ref contents 1) 1)))
    (check (cdr (assoc "thoughtSignature" p1)) => "QUJD")
  ) ;let
  ;; turn 3: model, toolCall 保留原签名
  (let ((p0 (part-of (vector-ref contents 3) 0)))
    (check (cdr (assoc "thoughtSignature" p0)) => "REVG")
  ) ;let
  ;; turn 5: glm 的 model turn：text + 补哨兵的 toolCall
  (let ((text-part (part-of (vector-ref contents 5) 0))
        (fc-part (part-of (vector-ref contents 5) 1))
       ) ;
    (check (cdr (assoc "text" text-part)) => "文件已存在。")
    (check (cdr (assoc "thoughtSignature" fc-part))
      =>
      "skip_thought_signature_validator"
    ) ;check
    (check
      (cdr (assoc "name" (cdr (assoc "functionCall" fc-part))))
      =>
      "bash"
    ) ;check
  ) ;let
) ;let

;; T2 目标为 Gemini 2.5：无签名 toolCall 不补哨兵（2.x 不强制签名）
(let ((contents (gemini-build-contents msgs "gemini-2.5-flash" "google")))
  (let ((fc-part (part-of (vector-ref contents 5) 1)))
    (check (assoc "thoughtSignature" fc-part) => #f)
  ) ;let
) ;let

;; T3 record 消息形态（会话读回路径）同样生效
(let ((contents
        (gemini-build-contents
          (list (pi-user-message "hi")
            (pi-assistant-message (list (pi-tool-call-block "tc-g" "bash" '() "QUJD"))
              "toolUse"
              #f
              #f
              #f
              "google"
              "gemini-3.8-flash"
              "google-generative-ai"
            ) ;pi-assistant-message
            (pi-tool-result-message "tc-g" "bash" (list (pi-text-block "ok")))
            (pi-assistant-message (list (pi-tool-call-block "tc-m" "bash" '()))
              "toolUse"
              #f
              #f
              #f
              "minimax-cn"
              "MiniMax-M3.1-Flash"
              "openai-completions"
            ) ;pi-assistant-message
            (pi-tool-result-message "tc-m" "bash" (list (pi-text-block "ok")))
          ) ;list
          "gemini-3.8-flash"
          "google"
        ) ;gemini-build-contents
      ) ;contents
     ) ;
  ;; gemini 自己的调用保留原签名
  (let ((p (part-of (vector-ref contents 1) 0)))
    (check (cdr (assoc "thoughtSignature" p)) => "QUJD")
  ) ;let
  ;; minimax 的调用补哨兵
  (let ((p (part-of (vector-ref contents 3) 0)))
    (check (cdr (assoc "thoughtSignature" p)) => "skip_thought_signature_validator")
  ) ;let
) ;let

(check-report)
