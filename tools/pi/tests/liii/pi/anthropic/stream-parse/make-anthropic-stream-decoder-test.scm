(import (liii check)
  (liii pi anthropic core)
  (liii pi anthropic stream-parse)
  (liii pi message)
) ;import

(check-set-mode! 'report-failed)

;; 1. 文本流式增量与结束
(let* ((evs '())
       (on-ev (lambda (e) (set! evs (cons e evs))))
       (stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
       (decoder (make-anthropic-stream-decoder stream-state on-ev))
      ) ;
  (decoder "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n")
  (decoder "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Hello\"}}\n\n")
  (decoder "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\" Anthropic\"}}\n\n")
  (decoder "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n")

  (check (length (vector-ref stream-state 0)) => 1)
  (check (pi-text-block-text (car (vector-ref stream-state 0))) => "Hello Anthropic")
) ;let*

;; 2. 普通 tool_use 流式参数拼接
(let* ((evs '())
       (on-ev (lambda (e) (set! evs (cons e evs))))
       (stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
       (decoder (make-anthropic-stream-decoder stream-state on-ev))
      ) ;
  (decoder "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"call_1\",\"name\":\"bash\",\"input\":{}}}\n\n")
  (decoder "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"command\\\":\\\"\"}}\n\n")
  (decoder "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"ls\\\"}\"}}\n\n")
  (decoder "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n")

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
  (decoder "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"call_empty\",\"name\":\"ls\",\"input\":{}}}\n\n")
  (decoder "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n")

  (check (length (vector-ref stream-state 0)) => 1)
  (let ((tc (car (vector-ref stream-state 0))))
    (check (pi-tool-call-block? tc) => #t)
    (check (pi-tool-call-block-id tc) => "call_empty")
    (check (pi-tool-call-block-name tc) => "ls")
    (check (pi-tool-call-block-arguments tc) => '())
  ) ;let
) ;let*

(check-report)
