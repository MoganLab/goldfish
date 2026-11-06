;; make-sse-event
;; 构造 Server-Sent Events (SSE) 事件记录对象。
;;
;; 语法
;; ----
;; (make-sse-event [event] data [id [retry]])
;;
;; 参数
;; ----
;; event : string? | #f (可选，默认 "message")
;; 事件类型名称。若为 #f 或空字符串则规范化为默认值 "message"。
;; data  : string?
;; 事件负载数据字符串。
;; id    : string? | #f (可选，默认 #f)
;; 事件唯一标识符。
;; retry : integer? | #f (可选，默认 #f)
;; 客户端重试连接等待时间（毫秒），必须为非负整数。
;;
;; 返回值
;; ----
;; <sse-event>
;; 新创建的 SSE 事件记录对象。
;;
;; 错误处理
;; ----
;; type-error 当参数个数不在 1 到 4 之间时。
;; type-error 当 event 不是 string? 或 #f 时。
;; type-error 当 data 不是 string? 时。
;; type-error 当 id 不是 string? 或 #f 时。
;; type-error 当 retry 不是 exact integer? 或 #f 时。
;; value-error 当 retry 为负整数时。

(import (scheme base) (liii check) (liii sse))

(check-set-mode! 'report-failed)

;; 1. 单参数构造（仅 data，event 默认为 "message"，id 和 retry 为 #f）
(let ((ev (make-sse-event "hello world")))
  (check-true (sse-event? ev))
  (check (sse-event-event ev) => "message")
  (check (sse-event-data ev) => "hello world")
  (check (sse-event-id ev) => #f)
  (check (sse-event-retry ev) => #f)
) ;let

;; 2. 两个参数构造（event 与 data）
(let ((ev (make-sse-event "greeting" "welcome!")))
  (check-true (sse-event? ev))
  (check (sse-event-event ev) => "greeting")
  (check (sse-event-data ev) => "welcome!")
  (check (sse-event-id ev) => #f)
  (check (sse-event-retry ev) => #f)
) ;let

;; 3. 三个参数构造（event, data, id）
(let ((ev (make-sse-event "ping" "pong" "msg-001")))
  (check-true (sse-event? ev))
  (check (sse-event-event ev) => "ping")
  (check (sse-event-data ev) => "pong")
  (check (sse-event-id ev) => "msg-001")
  (check (sse-event-retry ev) => #f)
) ;let

;; 4. 四个参数构造（event, data, id, retry）
(let ((ev (make-sse-event "notice" "server reload" "msg-002" 5000)))
  (check-true (sse-event? ev))
  (check (sse-event-event ev) => "notice")
  (check (sse-event-data ev) => "server reload")
  (check (sse-event-id ev) => "msg-002")
  (check (sse-event-retry ev) => 5000)
) ;let

;; 5. event 为 #f 或空字符串时自动规范化为 "message"
(let ((ev1 (make-sse-event #f "content" "id-1" 1000))
      (ev2 (make-sse-event "" "content" "id-2" 2000))
     ) ;
  (check (sse-event-event ev1) => "message")
  (check (sse-event-event ev2) => "message")
) ;let

;; 6. 构造异常与契约防御
(check-catch 'type-error (make-sse-event))
(check-catch 'type-error (make-sse-event "1" "2" "3" 4 5))
(check-catch 'type-error (make-sse-event 123 "data"))
(check-catch 'type-error (make-sse-event "event" 123))
(check-catch 'type-error (make-sse-event "event" "data" 123))
(check-catch 'type-error (make-sse-event "event" "data" "id" "retry-string"))
(check-catch 'value-error (make-sse-event "event" "data" "id" -1))
(check-catch 'value-error (make-sse-event "event" "data" "id" -500))

(check-report)
