(import (liii check) (liii sse) (scheme base))

(check-set-mode! 'report-failed)

;; make-sse-decoder
;; 构造增量 Chunk 解码器闭包，接收字符串分片并在事件闭合时触发回调。
;;
;; 语法
;; ----
;; (make-sse-decoder on-event) → procedure
;;
;; 参数
;; ----
;; on-event : procedure?
;; 接收一个参数 (lambda (event) ...) 的回调函数，当完整 SSE 事件闭合时被调用。
;;
;; 返回值
;; ----
;; procedure (lambda (chunk) ...)
;; 解码器闭包。接收 string? 或 eof-object? 类型的 chunk。
;;
;; 错误处理
;; ----
;; type-error 当 on-event 不是 procedure? 时。
;; type-error 当 chunk 既不是 string? 也不是 eof-object? 时。

;; 1. 基础单 Chunk 完整事件触发
(let* ((received '())
       (decoder
         (make-sse-decoder (lambda (ev) (set! received (cons ev received))))
       ) ;decoder
      ) ;
  (decoder "data: hello world\n\n")
  (check (length received) => 1)
  (let ((ev (car received)))
    (check (sse-event-event ev) => "message")
    (check (sse-event-data ev) => "hello world")
    (check (sse-event-id ev) => #f)
    (check (sse-event-retry ev) => #f)
  ) ;let
) ;let*

;; 2. 人为拆分数据包模拟 TCP 分片（断包与拼包测试）
(let* ((received '())
       (decoder
         (make-sse-decoder (lambda (ev) (set! received (cons ev received))))
       ) ;decoder
      ) ;
  ;; 第一次传入半包：不应触发回调
  (decoder "data: he")
  (check (length received) => 0)
  ;; 第二次传入后半部分并闭合
  (decoder "llo\n\n")
  (check (length received) => 1)
  (check (sse-event-data (car received)) => "hello")
) ;let*

;; 3. 跨 \r\n 换行符的分片测试
(let* ((received '())
       (decoder
         (make-sse-decoder (lambda (ev) (set! received (cons ev received))))
       ) ;decoder
      ) ;
  (decoder "event: update\r")
  (check (length received) => 0)
  (decoder "\ndata: payload\r\n\r")
  (check (length received) => 0)
  (decoder "\n")
  (check (length received) => 1)
  (let ((ev (car received)))
    (check (sse-event-event ev) => "update")
    (check (sse-event-data ev) => "payload")
  ) ;let
) ;let*

;; 4. 单 Chunk 内包含多条 SSE 消息
(let* ((received '())
       (decoder
         (make-sse-decoder (lambda (ev) (set! received (cons ev received))))
       ) ;decoder
      ) ;
  (decoder "data: first\n\ndata: second\n\ndata: third\n\n")
  (check (length received) => 3)
  (check (map sse-event-data (reverse received)) => '("first" "second" "third"))
) ;let*

;; 5. 多行连续 data 字段跨 Chunk 累积
(let* ((received '())
       (decoder
         (make-sse-decoder (lambda (ev) (set! received (cons ev received))))
       ) ;decoder
      ) ;
  (decoder "data: line 1\n")
  (decoder "data: line 2\n")
  (decoder "data: line 3\n\n")
  (check (length received) => 1)
  (check (sse-event-data (car received)) => "line 1\nline 2\nline 3")
) ;let*

;; 6. 注释与心跳行忽略，不触发回调
(let* ((received '())
       (decoder
         (make-sse-decoder (lambda (ev) (set! received (cons ev received))))
       ) ;decoder
      ) ;
  (decoder ": heartbeat ping\n\n")
  (check (length received) => 0)
  (decoder ": comment 1\n: comment 2\ndata: actual data\n\n")
  (check (length received) => 1)
  (check (sse-event-data (car received)) => "actual data")
) ;let*

;; 7. 事件状态重置（前一个事件的字段不污染后续事件）
(let* ((received '())
       (decoder
         (make-sse-decoder (lambda (ev) (set! received (cons ev received))))
       ) ;decoder
      ) ;
  (decoder "event: custom\nid: 99\nretry: 4000\ndata: with-fields\n\n")
  (decoder "data: next-plain-event\n\n")
  (check (length received) => 2)
  (let ((ev1 (cadr received)) (ev2 (car received)))
    (check (sse-event-event ev1) => "custom")
    (check (sse-event-id ev1) => "99")
    (check (sse-event-retry ev1) => 4000)
    (check (sse-event-event ev2) => "message")
    (check (sse-event-id ev2) => #f)
    (check (sse-event-retry ev2) => #f)
  ) ;let
) ;let*

;; 8. EOF 闭合信号冲刷残余未闭合事件
(let* ((received '())
       (decoder
         (make-sse-decoder (lambda (ev) (set! received (cons ev received))))
       ) ;decoder
       (eof (read-char (open-input-string "")))
      ) ;
  (decoder "data: unclosed-line")
  (check (length received) => 0)
  (decoder eof)
  (check (length received) => 1)
  (check (sse-event-data (car received)) => "unclosed-line")
) ;let*

;; 9. 错误处理
(check-catch 'type-error (make-sse-decoder "not-a-proc"))
(check-catch 'type-error (make-sse-decoder 123))

(let ((decoder (make-sse-decoder (lambda (ev) #f))))
  (check-catch 'type-error (decoder 123))
  (check-catch 'type-error (decoder 'chunk))
) ;let

(check-report)
