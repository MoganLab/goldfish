;; pi-settings-model-thinking-level
;; 获取指定模型在配置中单独覆盖的思考级别。
;;
;; 语法
;; ----
;; (pi-settings-model-thinking-level settings provider model-id)
;;
;; 参数
;; ----
;; settings : any?
;; 全局偏好配置 record 或 alist。
;;
;; provider : string?
;; 服务商名称。
;;
;; model-id : string?
;; 模型标识符。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 模型特定的思考级别字符串，未配置返回 #f。

(import (liii check) (liii pi ai config record))

(check-set-mode! 'report-failed)

(let* ((levels '(("deepseek/deepseek-v4-pro" . "high")))
       (s (pi-settings :model-thinking-levels levels))
      ) ;
  (check (pi-settings-model-thinking-level s "deepseek" "deepseek-v4-pro")
    =>
    "high"
  ) ;check
  (check (pi-settings-model-thinking-level s "google" "gemini-2.5-flash") => #f)
) ;let*

(check-report)
