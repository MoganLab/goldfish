;; make-stream-decoder
;; 创建 Gemini SSE 流解码器，把 SSE 数据块解析为内部消息块与事件。
;;
;; 语法
;; ----
;; (make-stream-decoder stream-state on-event)
;;
;; 参数
;; ----
;; stream-state : gemini-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; on-event : procedure | #f
;; 接收流式事件的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; procedure
;; 接收 SSE 字符串 chunk 的解码过程，可多次调用以处理连续数据流。

(import (liii check)
  (liii pi ai message)
  (liii pi ai api google-generative-ai stream-parse)
  (liii pi ai api google-generative-ai stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 文本流式增量
(let* ((stream-state (vector '() #f '() '() "STOP" #f "" ""))
       (events '())
       (emit
         (lambda (ev) (set! events (append events (list ev))))
       ) ;emit
       (decoder (make-stream-decoder stream-state emit))
      ) ;
  (decoder "data: {\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Hello\"}]},\"finishReason\":\"STOP\"}]}\n\n"
  ) ;decoder
  (check (length (gemini-stream-state-blocks stream-state)) => 1)
  (check (pi-text-block-text (car (gemini-stream-state-blocks stream-state)))
    =>
    "Hello"
  ) ;check
  (check (gemini-stream-state-finish-reason stream-state) => "STOP")
  (check (length events) => 2)
  (check (cdr (assoc 'type (car events))) => "text_start")
) ;let*

;; 非 JSON 数据被忽略
(let* ((stream-state (vector '() #f '() '() "STOP" #f "" ""))
       (events '())
       (emit
         (lambda (ev) (set! events (append events (list ev))))
       ) ;emit
       (decoder (make-stream-decoder stream-state emit))
      ) ;
  (decoder "data: not-a-json\n\n")
  (check (length (gemini-stream-state-blocks stream-state)) => 0)
  (check (length events) => 0)
) ;let*

;; 空数据被忽略
(let* ((stream-state (vector '() #f '() '() "STOP" #f "" ""))
       (decoder (make-stream-decoder stream-state #f))
      ) ;
  (decoder "data: \n\n")
  (check (length (gemini-stream-state-blocks stream-state)) => 0)
) ;let*

(check-report)
