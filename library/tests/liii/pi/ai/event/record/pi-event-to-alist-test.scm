;; pi-event->alist
;; 将事件 record 转换为用于序列化的关联列表（alist）。
;;
;; 语法
;; ----
;; (pi-event->alist ev)
;;
;; 参数
;; ----
;; ev : any?
;; 事件对象。
;;
;; 返回值
;; ----
;; alist?
;; 事件对应的关联列表表示。

(import (liii check) (liii pi ai event record))

(check-set-mode! 'report-failed)

(let* ((ev (pi-event-agent-start)) (al (pi-event->alist ev)))
  (check-true (pair? al))
  (check (cdr (assoc "type" al)) => "agent_start")
) ;let*

(check-report)
