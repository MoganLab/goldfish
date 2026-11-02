;; %poll-config-messages
;; 从 Agent 配置中调用指定的轮询回调，并安全提取排队消息列表。
;;
;; 语法
;; ----
;; (%poll-config-messages config key)
;;
;; 参数
;; ----
;; config : list?，包含轮询回调的配置 alist。
;; key : any?，配置项键名（如 'get-steering-messages 或 'get-follow-up-messages）。
;;
;; 返回值
;; ----
;; list?，排队消息列表；若回调不存在、抛错或返回非非空列表，则兜底返回 '()。

(import (scheme base) (liii check) (liii pi agent loop-queue))

(check-set-mode! 'report-failed)

;; 1. 回调正常返回非空消息列表
(let* ((cfg (list (cons 'poll-msg (lambda () '("msg1" "msg2"))))))
  (check (%poll-config-messages cfg 'poll-msg) => '("msg1" "msg2"))
) ;let*

;; 2. 回调返回空列表
(let* ((cfg (list (cons 'poll-msg (lambda () '())))))
  (check (%poll-config-messages cfg 'poll-msg) => '())
) ;let*

;; 3. 回调不存在
(check (%poll-config-messages '() 'poll-msg) => '())

;; 4. 回调内部抛出异常（防御性捕获并兜底为 '()）
(let* ((cfg (list (cons 'poll-msg (lambda () (error "poll error"))))))
  (check (%poll-config-messages cfg 'poll-msg) => '())
) ;let*

;; 5. 回调返回非列表对象（如数字或布尔值）
(let* ((cfg (list (cons 'poll-msg (lambda () 123)))))
  (check (%poll-config-messages cfg 'poll-msg) => '())
) ;let*

(check-report)
