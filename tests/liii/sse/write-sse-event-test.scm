(import (liii check) (liii sse) (scheme base))

(check-set-mode! 'report-failed)

;; sse-event->string / write-sse-event
;; 将 <sse-event> 记录序列化编码为遵循 W3C 规范的 SSE 文本分帧格式。
;;
;; 语法
;; ----
;; (sse-event->string event) → string
;; (write-sse-event event [port]) → unspecified
;;
;; 参数
;; ----
;; event : <sse-event> 实例
;; port  : output-port? (可选，默认 (current-output-port))
;;
;; 返回值
;; ----
;; sse-event->string 返回序列化后的标准 SSE 格式字符串。
;; write-sse-event 将结果输出至端口。
;;
;; 错误处理
;; ----
;; type-error 当 event 不是 <sse-event> 记录实例时。
;; type-error 当 port 不是 output-port? 时。

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

;; 5. write-sse-event 输出到指定字符串端口
(let ((p (open-output-string))
      (ev (make-sse-event "alert" "high load" "alert-1" #f))
     ) ;
  (write-sse-event ev p)
  (check (get-output-string p)
    =>
    "event: alert\ndata: high load\nid: alert-1\n\n"
  ) ;check
) ;let

;; 6. 编码与解码回环（Round-trip）校验
(let* ((ev-orig (make-sse-event "event-type" "payload line1\npayload line2" "id-123" 2500)
       ) ;ev-orig
       (str (sse-event->string ev-orig))
       (p (open-input-string str))
       (ev-parsed (read-sse-event p))
      ) ;
  (check (sse-event-event ev-parsed) => (sse-event-event ev-orig))
  (check (sse-event-data ev-parsed) => (sse-event-data ev-orig))
  (check (sse-event-id ev-parsed) => (sse-event-id ev-orig))
  (check (sse-event-retry ev-parsed) => (sse-event-retry ev-orig))
) ;let*

;; 7. 错误处理
(check-catch 'type-error (sse-event->string "not-an-event"))
(check-catch 'type-error (sse-event->string 123))

(check-catch 'type-error (write-sse-event "not-an-event"))
(let ((ev (make-sse-event "data")))
  (check-catch 'type-error (write-sse-event ev 123))
  (check-catch 'type-error (write-sse-event ev "not-a-port"))
  (check-catch 'type-error (write-sse-event ev (open-output-string) 'extra))
) ;let

(check-report)
