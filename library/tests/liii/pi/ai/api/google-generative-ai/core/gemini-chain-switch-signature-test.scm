;; [1325] all-in-one 切换 Gemini 3.8 的历史工具调用签名集成回归
;; 模拟 minimax-cn 节点留下 default_api:read 及 toolResult，
;; 后续请求切换到 google/gemini-3.8-flash。
;;
;; 原实现剥离异构签名后产生无签名 functionCall，触发 HTTP 400；
;; 修复后首个 functionCall 携带 Google 官方兼容哨兵，工具结果不携带签名。
;;
;; 语法
;; ----
;; (gemini-build-contents messages model-id provider)
;;
;; 参数
;; ----
;; messages : list
;; 包含 minimax-cn 工具调用、工具结果及后续用户消息的历史。
;; model-id、provider : string
;; 切换后的 google/gemini-3.8-flash 目标节点。
;;
;; 返回值
;; ----
;; vector
;; 首个 functionCall 带兼容哨兵、toolResult 不带签名的 Gemini contents。
;;
;; 说明
;; ----
;; 本文件是 gemini-build-contents 的跨节点集成回归，不访问真实网络。

(import (liii check)
  (liii pi ai api google-generative-ai core)
) ;import

(check-set-mode! 'report-failed)

(define history
  (list '((role . "user") (content . "read the source"))
    '((role . "assistant")
      (provider . "minimax-cn")
      (model . "MiniMax-M3.1-Flash")
      (content ((type . "toolCall")
                (id . "call_20")
                (name . "default_api:read")
                (arguments ("path" . "source.scm"))
                (thoughtSignature . "bWluaW1heA==")))
      (stopReason . "toolUse"))
    '((role . "toolResult")
      (toolCallId . "call_20")
      (toolName . "default_api:read")
      (isError . #f)
      (content ((type . "text") (text . "source"))))
    '((role . "user") (content . "continue"))
  ) ;list
) ;define

(define contents (gemini-build-contents history "gemini-3.8-flash" "google"))

(define (part-of content idx)
  (vector-ref (cdr (assoc "parts" content)) idx)
) ;define

(define model-part (part-of (vector-ref contents 1) 0))

(check (vector-length contents) => 4)
(check (cdr (assoc "name" (cdr (assoc "functionCall" model-part)))) => "default_api:read")
(check (cdr (assoc "thoughtSignature" model-part))
  =>
  "skip_thought_signature_validator"
) ;check
(check (assoc "thoughtSignature" (part-of (vector-ref contents 2) 0)) => #f)

(check-report)
