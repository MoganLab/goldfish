;; process-openai-tool-call-delta
;; 处理 OpenAI 流式 tool_calls 增量，累积参数并在首个分块派发 toolcall_start。
;;
;; 语法
;; ----
;; (process-openai-tool-call-delta tc stream-state on-event)
;;
;; 参数
;; ----
;; tc : alist
;; tool_calls 增量片段，包含 index，可选包含 id 与 function (name, arguments)。
;; stream-state : openai-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; on-event : procedure | #f
;; 接收流式事件的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新 stream-state 中的 tools 表。

(import (liii check)
  (liii pi ai message)
  (liii pi ai api openai-completions)
  (liii pi ai api openai-completions stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 首个 tool delta：记录并派发 toolcall_start
(let* ((stream-state (make-openai-stream-state '() #f '() "stop" #f '()))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
       (tc '(("index" . 0)
             ("id" . "call_1")
             ("function" . (("name" . "bash") ("arguments" . "{\"command\":")))))
      ) ;
  (process-openai-tool-call-delta tc stream-state emit)
  (check (length (openai-stream-state-tools stream-state)) => 1)
  (check (length events) => 2)
  (check (cdr (assoc 'type (car events))) => "toolcall_start")
  (check (cdr (assoc 'toolName (car events))) => "bash")
  (check (cdr (assoc 'type (cadr events))) => "toolcall_delta")
) ;let*

;; 后续参数增量：拼接并派发 toolcall_delta
(let* ((stream-state (make-openai-stream-state '() #f '() "stop" #f '()))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
       (tc1 '(("index" . 0)
              ("id" . "call_1")
              ("function" . (("name" . "bash") ("arguments" . "{\"command\":")))))
       (tc2 '(("index" . 0)
              ("function" . (("arguments" . "\"ls\"}")))))
      ) ;
  (process-openai-tool-call-delta tc1 stream-state emit)
  (set! events '())
  (process-openai-tool-call-delta tc2 stream-state emit)
  (check (length (openai-stream-state-tools stream-state)) => 1)
  (check (length events) => 1)
  (check (cdr (assoc 'type (car events))) => "toolcall_delta")
  (check (cdr (assoc 'delta (car events))) => "\"ls\"}")
) ;let*

(check-report)
