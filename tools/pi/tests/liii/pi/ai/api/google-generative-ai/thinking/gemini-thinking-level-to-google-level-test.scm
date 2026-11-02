;; gemini-thinking-level->google-level
;; 将内部思考级别映射为 Gemini API 的 thinkingLevel。
;;
;; 语法
;; ----
;; (gemini-thinking-level->google-level level)
;;
;; 参数
;; ----
;; level : string
;; 内部思考级别，可取 minimal、low、medium、high、xhigh 或 max。
;;
;; 返回值
;; ----
;; string | #f
;; Gemini 的 thinkingLevel；级别为 off 或未知值时返回 #f。

(import (liii check) (liii pi ai api google-generative-ai thinking))

(check-set-mode! 'report-failed)

(check (gemini-thinking-level->google-level "minimal") => "MINIMAL")
(check (gemini-thinking-level->google-level "low") => "LOW")
(check (gemini-thinking-level->google-level "medium") => "MEDIUM")
(check (gemini-thinking-level->google-level "high") => "HIGH")
(check (gemini-thinking-level->google-level "xhigh") => "HIGH")
(check (gemini-thinking-level->google-level "max") => "HIGH")
(check (gemini-thinking-level->google-level "off") => #f)
(check (gemini-thinking-level->google-level "unknown") => #f)

(check-report)
