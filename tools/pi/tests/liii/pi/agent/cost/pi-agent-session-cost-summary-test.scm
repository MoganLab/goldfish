(import (scheme base)
  (liii check)
  (liii pi agent cost)
) ;import

(check-set-mode! 'report-failed)

;; pi-agent-session-cost-summary
;; 汇总 agent 会话中 assistant 消息的调用费用与缓存节省。
;;
;; 语法
;; ----
;; (pi-agent-session-cost-summary msgs config)
;;
;; 参数
;; ----
;; msgs : list
;; 会话消息列表，元素可以是 alist 或消息 record。
;; config : alist
;; 包含 provider、model-id 和 models-store 的模型配置。
;;
;; 返回值
;; ----
;; alist
;; 包含 totalCost 和 totalSavings 两个数值字段。
;;
;; 说明
;; ----
;; 仅汇总带 usage 的 assistant 消息，兼容 symbol 和 string 键。
;; 其他角色、缺失 usage 的消息以及空会话不会产生费用。
;;
;; 示例
;; ----
;; (pi-agent-session-cost-summary messages config) => ((totalCost . 4.575) (totalSavings . 0.675))

;; 混合角色与两种键风格：分别累计 input、output、cacheRead 对应的费用和缓存节省
;; 仅汇总 assistant 消息中的 usage，忽略其他角色及缺失 usage 的消息
(let* ((config '((provider . "google")
                 (model-id . "gemini-3.8-flash")
                 (models-store . ())))
       (msgs (list '((role . "user")
                     (usage . ((input . 1000000))))
               '((role . "assistant")
                 (usage . ((input . 1000000)
                           (output . 0)
                           (cacheRead . 0)
                           (cacheWrite . 0))))
               '(("role" . "assistant")
                 ("usage" . ((input . 0)
                             (output . 1000000)
                             (cacheRead . 0)
                             (cacheWrite . 0))))
               '((role . "assistant")
                 (usage . ((input . 0)
                           (output . 0)
                           (cacheRead . 1000000)
                           (cacheWrite . 0))))
               '((role . "assistant"))
               '((role . "toolResult")
                 (usage . ((input . 1000000))))))
       (summary (pi-agent-session-cost-summary msgs config)))
  (check (cdr (assoc 'totalCost summary)) => 4.575)
  (check (cdr (assoc 'totalSavings summary)) => 0.675))

;; 空会话返回零费用和零缓存节省
(let ((summary (pi-agent-session-cost-summary
                 '()
                 '((provider . "google")
                   (model-id . "gemini-3.8-flash")
                   (models-store . ())))))
  (check (cdr (assoc 'totalCost summary)) => 0.0)
  (check (cdr (assoc 'totalSavings summary)) => 0.0))

(check-report)
