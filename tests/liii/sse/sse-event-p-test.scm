;; sse-event?
;; 判断给定对象是否为 Server-Sent Events (SSE) 事件记录。
;;
;; 语法
;; ----
;; (sse-event? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检测的对象。
;;
;; 返回值
;; ----
;; boolean
;; 当 obj 为 <sse-event> 记录时返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii sse))

(check-set-mode! 'report-failed)

;; 1. 有效事件记录判定
(check-true (sse-event? (make-sse-event "test")))
(check-true (sse-event? (make-sse-event "custom" "data" "id" 1000)))

;; 2. 非事件记录判定
(check-false (sse-event? "not-an-event"))
(check-false (sse-event? 12345))
(check-false (sse-event? '()))
(check-false (sse-event? #(1 2 3)))
(check-false (sse-event? #f))
(check-false (sse-event? #t))

(check-report)
