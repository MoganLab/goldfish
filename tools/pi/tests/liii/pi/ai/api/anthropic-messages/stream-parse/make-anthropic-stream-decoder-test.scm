(import (liii check)
  (liii pi ai api anthropic-messages core)
  (liii pi ai api anthropic-messages stream-parse)
  (liii pi ai message)
) ;import

;; make-anthropic-stream-decoder
;; 创建 Anthropic SSE 流解码器，将文本、工具调用和 thinking 增量写入流状态。
;;
;; 语法
;; ----
;; (make-anthropic-stream-decoder stream-state on-ev)
;;
;; 参数
;; ----
;; stream-state : anthropic-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; on-ev : procedure | #f
;; 接收 text、toolcall 和 thinking 事件 alist 的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; procedure
;; 接收 SSE 字符串 chunk 的解码过程，可多次调用以处理连续数据流。
;;
;; 说明
;; ----
;; 文本增量会合并到当前文本块，工具参数 JSON 增量会按块拼接。
;; content_block_stop 会结束当前块，空工具参数归一化为空 alist。
;;
;; 示例
;; ----
;; (define decoder (make-anthropic-stream-decoder stream-state on-ev))
;; (decoder "data: {\"type\":\"content_block_stop\",\"index\":0}\n\n")

(check-set-mode! 'report-failed)

;; 1. 文本流式增量与结束
(let* ((evs '())
       (on-ev (lambda (e) (set! evs (cons e evs))))
       (stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
       (decoder (make-anthropic-stream-decoder stream-state on-ev))
      ) ;
  (decoder "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
  ) ;decoder
  (decoder "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Hello\"}}\n\n"
  ) ;decoder
  (decoder "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\" Anthropic\"}}\n\n"
  ) ;decoder
  (decoder "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
  ) ;decoder

  (check (length (vector-ref stream-state 0)) => 1)
  (check (pi-text-block-text (car (vector-ref stream-state 0)))
    =>
    "Hello Anthropic"
  ) ;check
) ;let*

;; 2. 普通 tool_use 流式参数拼接
(let* ((evs '())
       (on-ev (lambda (e) (set! evs (cons e evs))))
       (stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
       (decoder (make-anthropic-stream-decoder stream-state on-ev))
      ) ;
  (decoder "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"call_1\",\"name\":\"bash\",\"input\":{}}}\n\n"
  ) ;decoder
  (decoder "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"command\\\":\\\"\"}}\n\n"
  ) ;decoder
  (decoder "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"ls\\\"}\"}}\n\n"
  ) ;decoder
  (decoder "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
  ) ;decoder

  (check (length (vector-ref stream-state 0)) => 1)
  (let ((tc (car (vector-ref stream-state 0))))
    (check (pi-tool-call-block? tc) => #t)
    (check (pi-tool-call-block-id tc) => "call_1")
    (check (pi-tool-call-block-name tc) => "bash")
    (check (cdr (assoc "command" (pi-tool-call-block-arguments tc))) => "ls")
  ) ;let
) ;let*

;; 3. 关键测试：tool_use 无参数 delta（如直接 stop），参数归一化为 '() 且不崩溃
(let* ((evs '())
       (on-ev (lambda (e) (set! evs (cons e evs))))
       (stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
       (decoder (make-anthropic-stream-decoder stream-state on-ev))
      ) ;
  (decoder "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"call_empty\",\"name\":\"ls\",\"input\":{}}}\n\n"
  ) ;decoder
  (decoder "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
  ) ;decoder

  (check (length (vector-ref stream-state 0)) => 1)
  (let ((tc (car (vector-ref stream-state 0))))
    (check (pi-tool-call-block? tc) => #t)
    (check (pi-tool-call-block-id tc) => "call_empty")
    (check (pi-tool-call-block-name tc) => "ls")
    (check (pi-tool-call-block-arguments tc) => '())
  ) ;let
) ;let*

(check-report)
