;; anthropic-thinking-level->budget
;; 将 Anthropic 思考级别映射为对应的 thinking 预算 token 数。
;;
;; 语法
;; ----
;; (anthropic-thinking-level->budget level)
;;
;; 参数
;; ----
;; level : string
;; 思考级别，可取 minimal、low、medium、high、xhigh 或 max。
;;
;; 返回值
;; ----
;; integer | #f
;; 对应的 budget_tokens；级别为 off 或未知值时返回 #f。

(import (liii check) (liii pi ai api anthropic-messages thinking))

(check-set-mode! 'report-failed)

(check (anthropic-thinking-level->budget "minimal") => 1024)
(check (anthropic-thinking-level->budget "low") => 2048)
(check (anthropic-thinking-level->budget "medium") => 8192)
(check (anthropic-thinking-level->budget "high") => 16384)
(check (anthropic-thinking-level->budget "xhigh") => 16384)
(check (anthropic-thinking-level->budget "max") => 16384)
(check (anthropic-thinking-level->budget "off") => #f)
(check (anthropic-thinking-level->budget "unknown") => #f)

(check-report)
