;; sse-event->string
;; 将 SSE 事件记录序列化为遵循 W3C 规范的标准 SSE 文本格式字符串。
;;
;; 语法
;; ----
;; (sse-event->string event)
;;
;; 参数
;; ----
;; event : <sse-event>
;; 待序列化的 SSE 事件记录对象。
;;
;; 返回值
;; ----
;; string
;; 序列化后的标准 SSE 格式字符串。
;;
;; 错误处理
;; ----
;; type-error 当 event 不是 <sse-event> 记录时。

(import (scheme base) (liii check) (liii sse))

(check-set-mode! 'report-failed)

;; 1. 默认 event ("message") 不输出 event: 字段行
(let ((ev (make-sse-event "hello world")))
  (check (sse-event->string ev) => "data: hello world\n\n")
) ;let

;; 2. 自定义 event 输出 event: 行
(let ((ev (make-sse-event "notification" "update available")))
  (check (sse-event->string ev)
    =>
    "event: notification\ndata: update available\n\n"
  ) ;check
) ;let

;; 3. 包含完整 id 与 retry 字段
(let ((ev (make-sse-event "ping" "pong" "msg-99" 5000)))
  (check (sse-event->string ev)
    =>
    "event: ping\ndata: pong\nid: msg-99\nretry: 5000\n\n"
  ) ;check
) ;let

;; 4. 多行 data 内容拆解为多个连续 data: 字段
(let ((ev (make-sse-event "line 1\nline 2\nline 3")))
  (check (sse-event->string ev) => "data: line 1\ndata: line 2\ndata: line 3\n\n")
) ;let

;; 5. 错误处理
(check-catch 'type-error (sse-event->string "not-an-event"))
(check-catch 'type-error (sse-event->string 123))

(check-report)
