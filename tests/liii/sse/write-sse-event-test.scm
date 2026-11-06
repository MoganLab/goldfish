;; write-sse-event
;; 将 SSE 事件记录序列化并写出到指定的输出端口。
;;
;; 语法
;; ----
;; (write-sse-event event [port])
;;
;; 参数
;; ----
;; event : <sse-event>
;; 待序列化输出的 SSE 事件记录对象。
;; port  : output-port? (可选，默认 (current-output-port))
;; 目标输出端口。
;;
;; 返回值
;; ----
;; unspecified
;; 将结果写出到目标端口。
;;
;; 错误处理
;; ----
;; type-error 当 event 不是 <sse-event> 记录时。
;; type-error 当 port 不是 output-port? 时。

(import (scheme base) (liii check) (liii sse))

(check-set-mode! 'report-failed)

;; 1. write-sse-event 输出到指定字符串端口
(let ((p (open-output-string))
      (ev (make-sse-event "alert" "high load" "alert-1" #f))
     ) ;
  (write-sse-event ev p)
  (check (get-output-string p)
    =>
    "event: alert\ndata: high load\nid: alert-1\n\n"
  ) ;check
) ;let

;; 2. 默认输出端口与 round-trip
(let* ((ev-orig (make-sse-event "event-type" "payload line1\npayload line2" "id-123" 2500)
       ) ;ev-orig
       (p (open-output-string))
      ) ;
  (write-sse-event ev-orig p)
  (let* ((str (get-output-string p))
         (in-p (open-input-string str))
         (ev-parsed (read-sse-event in-p))
        ) ;
    (check (sse-event-event ev-parsed) => (sse-event-event ev-orig))
    (check (sse-event-data ev-parsed) => (sse-event-data ev-orig))
    (check (sse-event-id ev-parsed) => (sse-event-id ev-orig))
    (check (sse-event-retry ev-parsed) => (sse-event-retry ev-orig))
  ) ;let*
) ;let*

;; 3. 错误处理
(check-catch 'type-error (write-sse-event "not-an-event"))
(let ((ev (make-sse-event "data")))
  (check-catch 'type-error (write-sse-event ev 123))
  (check-catch 'type-error (write-sse-event ev "not-a-port"))
  (check-catch 'type-error (write-sse-event ev (open-output-string) 'extra))
) ;let

(check-report)
