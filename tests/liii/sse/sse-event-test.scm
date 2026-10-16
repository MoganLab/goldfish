(import (liii check) (liii sse) (liii json) (scheme base))

(check-set-mode! 'report-failed)

;; make-sse-event / sse-event? / sse-event-event / sse-event-data / sse-event-id / sse-event-retry / sse-event-json
;; Server-Sent Events (SSE) 事件记录类型及相关字段访问与 JSON 反序列化辅助函数。
;;
;; 语法
;; ----
;; (make-sse-event [event] data [id [retry]])
;; (sse-event? obj)
;; (sse-event-event ev)
;; (sse-event-data ev)
;; (sse-event-id ev)
;; (sse-event-retry ev)
;; (sse-event-json ev)
;;
;; 参数
;; ----
;; event : string? | #f (可选，默认 "message")
;; data  : string?
;; id    : string? | #f (可选，默认 #f)
;; retry : integer? | #f (可选，默认 #f，必须为非负整数)
;;
;; 错误处理
;; ----
;; type-error 当参数类型不匹配时。
;; value-error 当 retry 为负整数时。

;; 1. 基础单参数构造（仅提供 data，默认 event="message", id=#f, retry=#f）
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

;; 5. event 为 #f 或空字符串时自动回退规范化为 "message"
(let ((ev1 (make-sse-event #f "content" "id-1" 1000))
      (ev2 (make-sse-event "" "content" "id-2" 2000))
     ) ;
  (check (sse-event-event ev1) => "message")
  (check (sse-event-event ev2) => "message")
) ;let

;; 6. 类型谓词判定
(check-true (sse-event? (make-sse-event "test")))
(check-false (sse-event? "not-an-event"))
(check-false (sse-event? 12345))
(check-false (sse-event? '()))
(check-false (sse-event? #(1 2 3)))
(check-false (sse-event? #f))

;; 7. sse-event-json 辅助函数
(let* ((ev (make-sse-event "json" "{\"user\":\"alice\",\"score\":99}"))
       (j (sse-event-json ev))
      ) ;
  (check (json-ref j "user") => "alice")
  (check (json-ref j "score") => 99)
) ;let*

;; 8. sse-event-json 非法调用报错
(check-catch 'type-error (sse-event-json "not-an-event"))
(check-catch 'type-error (sse-event-json 42))

;; 9. 构造异常与契约防御
(check-catch 'type-error (make-sse-event))
(check-catch 'type-error (make-sse-event "1" "2" "3" 4 5))
(check-catch 'type-error (make-sse-event 123 "data"))
(check-catch 'type-error (make-sse-event "event" 123))
(check-catch 'type-error (make-sse-event "event" "data" 123))
(check-catch 'type-error (make-sse-event "event" "data" "id" "retry-string"))
(check-catch 'value-error (make-sse-event "event" "data" "id" -1))
(check-catch 'value-error (make-sse-event "event" "data" "id" -500))

(check-report)
