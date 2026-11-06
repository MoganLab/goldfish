;; sse-event-retry
;; 获取 SSE 事件记录中的客户端重试间隔毫秒数。
;;
;; 语法
;; ----
;; (sse-event-retry event)
;;
;; 参数
;; ----
;; event : <sse-event>
;; 待查询的 SSE 事件记录对象。
;;
;; 返回值
;; ----
;; integer? | #f
;; 重试间隔毫秒数；若未设置则返回 #f。

(import (scheme base) (liii check) (liii sse))

(check-set-mode! 'report-failed)

;; 1. 未设置 retry 时返回 #f
(let ((ev (make-sse-event "data")))
  (check (sse-event-retry ev) => #f)
) ;let

;; 2. 获取已设置的 retry 毫秒数
(let ((ev (make-sse-event "msg" "data" "id-1" 3000)))
  (check (sse-event-retry ev) => 3000)
) ;let

(check-report)
