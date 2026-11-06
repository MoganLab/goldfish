;; sse-event-id
;; 获取 SSE 事件记录中的事件唯一标识符。
;;
;; 语法
;; ----
;; (sse-event-id event)
;;
;; 参数
;; ----
;; event : <sse-event>
;; 待查询的 SSE 事件记录对象。
;;
;; 返回值
;; ----
;; string? | #f
;; 事件 ID 字符串；若未设置则返回 #f。

(import (scheme base) (liii check) (liii sse))

(check-set-mode! 'report-failed)

;; 1. 未设置 ID 时返回 #f
(let ((ev (make-sse-event "data")))
  (check (sse-event-id ev) => #f)
) ;let

;; 2. 获取已设置的 ID
(let ((ev (make-sse-event "update" "data" "id-42")))
  (check (sse-event-id ev) => "id-42")
) ;let

(check-report)
