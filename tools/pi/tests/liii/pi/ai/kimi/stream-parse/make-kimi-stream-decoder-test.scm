(import (liii check) (liii pi ai event) (liii pi ai kimi) (liii pi ai message))

(check-set-mode! 'report-failed)

;; make-kimi-stream-decoder
;; 测试纯文本流式增量解析与事件派发
(let* ((events '())
       (on-ev (lambda (ev) (set! events (cons ev events))))
       (stream-state (vector '() #f '() "stop" #f '()))
       (decoder (make-kimi-stream-decoder stream-state on-ev)))
  ;; 传入流式 chunk
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"Hello\"}}]}\n\n")
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\" world\"}}]}\n\n")
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\",\"usage\":{\"prompt_tokens\":10,\"completion_tokens\":5,\"total_tokens\":15}}]}\n\n")
  (decoder "data: [DONE]\n\n")

  (check (length (vector-ref stream-state 0)) => 1)
  (check (pi-text-block-text (car (vector-ref stream-state 0))) => "Hello world")
  (check (vector-ref stream-state 3) => "stop")
  (check (cdr (assoc 'totalTokens (vector-ref stream-state 2))) => 15))

;; 测试流式 tool_calls 增量解析与拼接
(let* ((events '())
       (on-ev (lambda (ev) (set! events (cons ev events))))
       (stream-state (vector '() #f '() "stop" #f '()))
       (decoder (make-kimi-stream-decoder stream-state on-ev)))
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call_1\",\"type\":\"function\",\"function\":{\"name\":\"bash\",\"arguments\":\"\"}}]}}]}\n\n")
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"{\\\"command\\\":\"}}]}}]}\n\n")
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"\\\"ls\\\"}\"}}]}}]}\n\n")
  (decoder "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"tool_calls\"}]}\n\n")
  (decoder "data: [DONE]\n\n")

  (check (length (vector-ref stream-state 0)) => 1)
  (let ((tc (car (vector-ref stream-state 0))))
    (check (pi-tool-call-block? tc) => #t)
    (check (pi-tool-call-block-id tc) => "call_1")
    (check (pi-tool-call-block-name tc) => "bash")
    (check (cdr (assoc "command" (pi-tool-call-block-arguments tc))) => "ls"))
  (check (vector-ref stream-state 3) => "toolUse"))

(check-report)
