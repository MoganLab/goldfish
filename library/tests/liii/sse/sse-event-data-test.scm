;; sse-event-data
;; 获取 SSE 事件记录中的负载数据字符串。
;;
;; 语法
;; ----
;; (sse-event-data event)
;;
;; 参数
;; ----
;; event : <sse-event>
;; 待查询的 SSE 事件记录对象。
;;
;; 返回值
;; ----
;; string
;; 事件负载数据字符串。

(import (scheme base) (liii check) (liii sse))

(check-set-mode! 'report-failed)

;; 1. 简单数据获取
(let ((ev (make-sse-event "hello world")))
  (check (sse-event-data ev) => "hello world")
) ;let

;; 2. 多行数据获取
(let ((ev (make-sse-event "line1\nline2\nline3")))
  (check (sse-event-data ev) => "line1\nline2\nline3")
) ;let

(check-report)
