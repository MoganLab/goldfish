;; pi-build-json-event
;; 根据事件类型统一分发构造对应的事件对象。
;;
;; 语法
;; ----
;; (pi-build-json-event type args)
;;
;; 参数
;; ----
;; type : any?
;; 事件类型标识。
;;
;; args : any?
;; 事件参数。
;;
;; 返回值
;; ----
;; any?
;; 构造出的事件对象。

(import (liii check) (liii pi ai event))

(check-set-mode! 'report-failed)

(let ((ev (pi-build-json-event 'agent_start '())))
  (check (cdr (assoc "type" ev)) => "agent_start")
) ;let

(check-report)
