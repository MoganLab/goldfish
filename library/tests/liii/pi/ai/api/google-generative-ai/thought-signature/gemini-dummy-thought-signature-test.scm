;; gemini-dummy-thought-signature
;; Google 官方用于无法保留真实签名场景的 thought signature 兼容哨兵。
;;
;; 语法
;; ----
;; gemini-dummy-thought-signature
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; string
;; "skip_thought_signature_validator"。
;;
;; 说明
;; ----
;; 跨模型历史或客户端自造 functionCall 没有真实签名时，
;; Gemini 3+ 可用该值跳过签名校验，而不是回放其他模型的签名。

(import (liii check) (liii pi ai api google-generative-ai thought-signature))

(check-set-mode! 'report-failed)

(check gemini-dummy-thought-signature => "skip_thought_signature_validator")

(check-report)
