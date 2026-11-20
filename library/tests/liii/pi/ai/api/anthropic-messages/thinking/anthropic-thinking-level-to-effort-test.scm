;; anthropic-thinking-level->effort
;; 将思考级别映射为自适应思考的 output_config.effort 档位。
;;
;; 语法
;; ----
;; (anthropic-thinking-level->effort level)
;;
;; 参数
;; ----
;; level : string
;; 思考级别，可取 minimal、low、medium、high、xhigh 或 max。
;;
;; 返回值
;; ----
;; string | #f
;; 对应的 effort 档位；级别为 off 或未知值时返回 #f。
;;
;; 说明
;; ----
;; effort 没有 minimal 档，minimal 映射为 low。

(import (liii check) (liii pi ai api anthropic-messages thinking))

(check-set-mode! 'report-failed)

(check (anthropic-thinking-level->effort "minimal") => "low")
(check (anthropic-thinking-level->effort "low") => "low")
(check (anthropic-thinking-level->effort "medium") => "medium")
(check (anthropic-thinking-level->effort "high") => "high")
(check (anthropic-thinking-level->effort "xhigh") => "xhigh")
(check (anthropic-thinking-level->effort "max") => "max")
(check (anthropic-thinking-level->effort "off") => #f)
(check (anthropic-thinking-level->effort "unknown") => #f)
(check (anthropic-thinking-level->effort #f) => #f)

(check-report)
