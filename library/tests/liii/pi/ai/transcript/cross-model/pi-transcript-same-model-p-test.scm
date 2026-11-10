;; pi-transcript-same-model?
;; 判定历史助手消息是否与当前请求目标属于同一模型（对标上游 isSameModel）。
;;
;; 语法
;; ----
;; (pi-transcript-same-model? msg config)
;;
;; 参数
;; ----
;; msg : alist
;; 历史助手消息关联表，可携带 provider、model、api 字段。
;; config : alist
;; 当前请求配置关联表，包含 provider、model-id、provider-api 等键。
;;
;; 返回值
;; ----
;; boolean?
;; model 完全匹配（且 provider、api 在两侧均有值时同样匹配）返回 #t；
;; 缺少模型元数据或任一维度不匹配时返回 #f。

(import (liii check) (liii pi ai transcript cross-model))

(check-set-mode! 'report-failed)

(define cfg
  '((provider . "google")
    (model-id . "gemini-2.5-flash")
    (provider-api . "google-generative-ai")
   )
) ;define

(define (mk-msg prov model api)
  (append
    (list (cons "role" "assistant") (cons "content" '()))
    (if prov (list (cons "provider" prov)) '())
    (if model (list (cons "model" model)) '())
    (if api (list (cons "api" api)) '())
  )
) ;define

;; 三者全部匹配 → 同模型
(check (pi-transcript-same-model?
         (mk-msg "google" "gemini-2.5-flash" "google-generative-ai")
         cfg
       ) => #t)

;; 字符串键形式的消息字段同样可识别
(check (pi-transcript-same-model?
         '(("role" . "assistant")
           ("provider" . "google")
           ("model" . "gemini-2.5-flash")
           ("api" . "google-generative-ai")
          )
         cfg
       ) => #t)

;; provider 不同 → 跨模型
(check (pi-transcript-same-model?
         (mk-msg "minimax-cn" "gemini-2.5-flash" "google-generative-ai")
         cfg
       ) => #f)

;; model 不同 → 跨模型
(check (pi-transcript-same-model?
         (mk-msg "google" "gemini-3-flash-preview" "google-generative-ai")
         cfg
       ) => #f)

;; api 不同 → 跨模型
(check (pi-transcript-same-model?
         (mk-msg "google" "gemini-2.5-flash" "anthropic-messages")
         cfg
       ) => #f)

;; 缺少模型元数据 → 不视为同模型
(check (pi-transcript-same-model? (mk-msg #f #f #f) cfg) => #f)
(check (pi-transcript-same-model? (mk-msg "google" #f #f) cfg) => #f)

;; 仅 provider 缺失、model 一致 → 视为同模型（历史会话可能未记录 provider）
(check (pi-transcript-same-model? (mk-msg #f "gemini-2.5-flash" #f) cfg) => #t)

;; 配置缺少目标模型 → 不视为同模型
(check (pi-transcript-same-model?
         (mk-msg "google" "gemini-2.5-flash" "google-generative-ai")
         '((provider . "google") (provider-api . "google-generative-ai"))
       ) => #f)

(check-report)
