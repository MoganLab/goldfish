(import (liii check)
  (liii pi ai event)
  (liii pi ai api openai-completions)
  (liii pi ai message)
) ;import

;; make-openai-stream-decoder
;; 创建 OpenAI Chat Completions SSE 流解码器，累积文本和工具调用增量。
;;
;; 语法
;; ----
;; (make-openai-stream-decoder stream-state on-ev)
;;
;; 参数
;; ----
;; stream-state : openai-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; on-ev : procedure | #f
;; 接收 text 和 toolcall 事件 alist 的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; procedure
;; 接收 SSE 字符串 chunk 的解码过程，可多次调用以处理连续数据流。
;;
;; 说明
;; ----
;; 同一文本块的增量会合并，工具调用按 index 聚合 id、name 和 arguments。
;; 收到 [DONE] 时完成所有未结束的工具调用。
;;
;; 示例
;; ----
;; (define decoder (make-openai-stream-decoder stream-state on-ev))
;; (decoder "data: [DONE]\n\n")

(check-set-mode! 'report-failed)

;; make-openai-stream-decoder
;; 测试纯文本流式增量解析与事件派发
(let* ((events '())
       (on-ev (lambda (ev) (set! events (cons ev events))))
       (stream-state (vector '() #f '() "stop" #f '()))
       (decoder (make-openai-stream-decoder stream-state on-ev))
      ) ;
  ;; 传入流式 chunk
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"Hello\"}}]}\n\n"
  ) ;decoder
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\" world\"}}]}\n\n"
  ) ;decoder
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\",\"usage\":{\"prompt_tokens\":10,\"completion_tokens\":5,\"total_tokens\":15}}]}\n\n"
  ) ;decoder
  (decoder "data: [DONE]\n\n")

  (check (length (vector-ref stream-state 0)) => 1)
  (check (pi-text-block-text (car (vector-ref stream-state 0))) => "Hello world")
  (check (vector-ref stream-state 3) => "stop")
  (check (cdr (assoc 'totalTokens (vector-ref stream-state 2))) => 15)
) ;let*

;; 测试流式 tool_calls 增量解析与拼接
(let* ((events '())
       (on-ev (lambda (ev) (set! events (cons ev events))))
       (stream-state (vector '() #f '() "stop" #f '()))
       (decoder (make-openai-stream-decoder stream-state on-ev))
      ) ;
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call_1\",\"type\":\"function\",\"function\":{\"name\":\"bash\",\"arguments\":\"\"}}]}}]}\n\n"
  ) ;decoder
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"{\\\"command\\\":\"}}]}}]}\n\n"
  ) ;decoder
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"\\\"ls\\\"}\"}}]}}]}\n\n"
  ) ;decoder
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"tool_calls\"}]}\n\n"
  ) ;decoder
  (decoder "data: [DONE]\n\n")

  (check (length (vector-ref stream-state 0)) => 1)
  (let ((tc (car (vector-ref stream-state 0))))
    (check (pi-tool-call-block? tc) => #t)
    (check (pi-tool-call-block-id tc) => "call_1")
    (check (pi-tool-call-block-name tc) => "bash")
    (check (cdr (assoc "command" (pi-tool-call-block-arguments tc))) => "ls")
  ) ;let
  (check (vector-ref stream-state 3) => "toolUse")
) ;let*

(check-report)
