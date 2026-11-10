;; pi-resolve-thinking-level
;; 解析最终生效的思考级别（CLI > per-model > 全局 > 默认）。
;;
;; 语法
;; ----
;; (pi-resolve-thinking-level cli-level model-level global-level available-levels)
;;
;; 参数
;; ----
;; cli-level : any?
;; 命令行指定的级别（或 #f）。
;;
;; model-level : any?
;; 模型特定配置的级别（或 #f）。
;;
;; global-level : any?
;; 全局设置的级别（或 #f）。
;;
;; available-levels : list?
;; 模型支持的级别列表。
;;
;; 返回值
;; ----
;; string?
;; 解析并收敛后的最终思考级别。

(import (liii check) (liii pi ai thinking))

(check-set-mode! 'report-failed)

(define %levels '("off" "minimal" "low" "medium" "high" "xhigh" "max"))

;; 1. CLI 优先
(check (pi-resolve-thinking-level "low" "high" "minimal" %levels) => "low")

;; 2. 无 CLI 时 per-model 覆盖优先于全局
(check (pi-resolve-thinking-level #f "high" "minimal" %levels) => "high")

;; 3. 只有全局默认时取全局
(check (pi-resolve-thinking-level #f #f "minimal" %levels) => "minimal")

;; 4. 都没有时取默认级别 medium
(check (pi-resolve-thinking-level #f #f #f %levels) => "medium")

;; 5. 空串视为未设置
(check (pi-resolve-thinking-level "" "" "" %levels) => "medium")

;; 6. 最终按模型可用级别收敛
(check (pi-resolve-thinking-level #f #f #f '("off")) => "off")

;; 7. 请求级别超出可用集时收敛到最近的支持级别
(check (pi-resolve-thinking-level "max" #f #f '("off" "minimal" "low" "medium"
                                                "high"))
  =>
  "high"
) ;check

(check-report)
