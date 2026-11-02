;; pi-agent-attach-cost
;; 根据模型费率表计算单次响应的费用并附加到 usage 字段中。
;;
;; 语法
;; ----
;; (pi-agent-attach-cost resp config)
;;
;; 参数
;; ----
;; resp : (or list? pi-assistant-message?)
;; 待附加费用的响应对象，支持 alist 格式或 pi-assistant-message record。
;; config : list?
;; 包含 provider、model-id 以及 models-store 的配置 alist。
;;
;; 返回值
;; ----
;; (or list? pi-assistant-message?)
;; 附加了 (cost . <alist>) 字段的更新后响应对象；若无 usage 则原样返回。

(import (scheme base) (liii check) (liii pi ai message) (liii pi agent cost))

(check-set-mode! 'report-failed)

;; 1. 非 pair 且非 record 的输入原样返回
(check (pi-agent-attach-cost "not-a-message" '()) => "not-a-message")
(check (pi-agent-attach-cost 123 '()) => 123)

;; 2. 无 usage 字段的消息原样返回
(let ((resp '((role . "assistant") (content . "hello"))))
  (check (pi-agent-attach-cost resp '()) => resp)
) ;let

;; 3. alist 格式消息包含 usage：计算并附加 cost
(let* ((config '((provider . "google")
                 (model-id . "gemini-3.8-flash")
                 (models-store)))
       (resp '((role . "assistant")
               (usage (input . 1000000)
                 (output . 0)
                 (cacheRead . 0)
                 (cacheWrite . 0)))
       ) ;resp
       (result (pi-agent-attach-cost resp config))
       (usage (cdr (assoc 'usage result)))
       (cost-info (cdr (assoc 'cost usage)))
      ) ;
  (check (cdr (assoc 'total cost-info)) => 0.75)
) ;let*

;; 4. pi-assistant-message record 包含 usage：计算并更新 usage 中的 cost
(let* ((config '((provider . "google")
                 (model-id . "gemini-3.8-flash")
                 (models-store)))
       (msg (pi-assistant-message "hello"
              :usage
              '((input . 1000000) (output . 0) (cacheRead . 0) (cacheWrite . 0))
            ) ;pi-assistant-message
       ) ;msg
       (result (pi-agent-attach-cost msg config))
      ) ;
  (check-true (pi-assistant-message? result))
  (let* ((usage (pi-assistant-message-usage result))
         (cost-info (cdr (assoc 'cost usage)))
        ) ;
    (check (cdr (assoc 'total cost-info)) => 0.75)
  ) ;let*
) ;let*

(check-report)
