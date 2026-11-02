;; gemini-build-thinking-config
;; 按思考级别构造 Gemini 请求的 thinkingConfig 片段。
;;
;; 语法
;; ----
;; (gemini-build-thinking-config level)
;;
;; 参数
;; ----
;; level : string? | #f
;; 思考级别，可取 minimal、low、medium、high、xhigh 或 max。
;;
;; 返回值
;; ----
;; alist | #f
;; 包含 thinkingConfig 的 alist；级别不受支持时返回 #f。

(import (liii check) (liii pi ai api google-generative-ai thinking))

(check-set-mode! 'report-failed)

(check (gemini-build-thinking-config "off") => #f)
(check (gemini-build-thinking-config #f) => #f)

(let ((cfg (gemini-build-thinking-config "low")))
  (check-true (pair? cfg))
  (let ((tc (cdr (assoc "thinkingConfig" cfg))))
    (check (cdr (assoc "includeThoughts" tc)) => #t)
    (check (cdr (assoc "thinkingLevel" tc)) => "LOW")
  ) ;let
) ;let

(let ((cfg (gemini-build-thinking-config "high")))
  (check-true (pair? cfg))
  (let ((tc (cdr (assoc "thinkingConfig" cfg))))
    (check (cdr (assoc "includeThoughts" tc)) => #t)
    (check (cdr (assoc "thinkingLevel" tc)) => "HIGH")
  ) ;let
) ;let

(check-report)
