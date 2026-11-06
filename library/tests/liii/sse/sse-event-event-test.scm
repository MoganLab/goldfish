;; sse-event-event
;; 获取 SSE 事件记录中的事件类型名称。
;;
;; 语法
;; ----
;; (sse-event-event event)
;;
;; 参数
;; ----
;; event : <sse-event>
;; 待查询的 SSE 事件记录对象。
;;
;; 返回值
;; ----
;; string
;; 事件类型名称（默认 "message"）。

(import (scheme base) (liii check) (liii sse))

(check-set-mode! 'report-failed)

;; 1. 默认事件类型
(let ((ev (make-sse-event "payload")))
  (check (sse-event-event ev) => "message")
) ;let

;; 2. 自定义事件类型
(let ((ev (make-sse-event "chat" "payload")))
  (check (sse-event-event ev) => "chat")
) ;let

;; 3. 规范化为默认值
(let ((ev (make-sse-event "" "payload")))
  (check (sse-event-event ev) => "message")
) ;let

(check-report)
