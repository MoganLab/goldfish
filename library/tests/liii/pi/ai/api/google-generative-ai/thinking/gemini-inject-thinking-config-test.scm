;; gemini-inject-thinking-config
;; 按思考级别向 Gemini 请求体注入 generationConfig.thinkingConfig。
;;
;; 语法
;; ----
;; (gemini-inject-thinking-config req-body level)
;;
;; 参数
;; ----
;; req-body : alist
;; Gemini 请求体 alist。
;; level : string? | #f
;; 思考级别；不受支持时保持请求体不变。
;;
;; 返回值
;; ----
;; alist
;; 注入 thinkingConfig 后的请求体。

(import (liii check) (liii pi ai api google-generative-ai thinking))

(check-set-mode! 'report-failed)

(let* ((base '(("contents" . #())))
       (injected (gemini-inject-thinking-config base "low"))
      ) ;
  (check-true (pair? (assoc "generationConfig" injected)))
  (let* ((gc (cdr (assoc "generationConfig" injected)))
         (tc (cdr (assoc "thinkingConfig" gc)))
        ) ;
    (check (cdr (assoc "includeThoughts" tc)) => #t)
    (check (cdr (assoc "thinkingLevel" tc)) => "LOW")
  ) ;let*
) ;let*

(let* ((base '(("contents" . #())))
       (injected (gemini-inject-thinking-config base "off"))
      ) ;
  (check-false (assoc "generationConfig" injected))
  (check injected => base)
) ;let*

(check-report)
