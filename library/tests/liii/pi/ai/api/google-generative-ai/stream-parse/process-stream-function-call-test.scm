;; process-stream-function-call
;; 处理 Gemini 流式响应中的 functionCall，写入工具调用块并派发工具事件。
;;
;; 语法
;; ----
;; (process-stream-function-call fc blocks stream-state on-event)
;;
;; 参数
;; ----
;; fc : alist
;; functionCall 对象，包含 name、args，可选 id 与 thoughtSignature。
;; blocks : list
;; 当前已完成的消息块列表。
;; stream-state : gemini-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; on-event : procedure | #f
;; 接收流式事件的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用追加 toolCall 块并更新工具调用列表。
;;
;; 说明
;; ----
;; 缺少 id 时按工具名与块数量生成规范化 id。

(import (liii check)
  (liii pi ai message)
  (liii pi ai api google-generative-ai stream-parse)
  (liii pi ai api google-generative-ai stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 带 id 的函数调用
(let* ((stream-state (vector '() #f '((input . 10) (output . 5)) '() "STOP" #f "" ""))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
      ) ;
  (process-stream-function-call '(("name" . "bash")
                                  ("args" . (("command" . "ls")))
                                  ("id" . "call_1"))
    '()
    stream-state
    emit
  ) ;process-stream-function-call
  (check (length (gemini-stream-state-blocks stream-state)) => 1)
  (let ((tc (car (gemini-stream-state-blocks stream-state))))
    (check (pi-tool-call-block? tc) => #t)
    (check (pi-tool-call-block-id tc) => "call_1")
    (check (pi-tool-call-block-name tc) => "bash")
    (check (cdr (assoc "command" (pi-tool-call-block-arguments tc))) => "ls")
  ) ;let
  (check (length (gemini-stream-state-tool-calls stream-state)) => 1)
  (check (gemini-stream-state-last-text stream-state) => #f)
  (check (length events) => 3)
  (check (cdr (assoc 'type (car events))) => "toolcall_start")
  (check (cdr (assoc 'id (car events))) => "call_1")
  (check (cdr (assoc 'type (cadr events))) => "toolcall_delta")
  (check (cdr (assoc 'type (caddr events))) => "toolcall_end")
) ;let*

;; 缺少 id 时生成规范化 id
(let ((stream-state (vector '() #f '() '() "STOP" #f "" "")))
  (process-stream-function-call '(("name" . "bash") ("args" . ())) '() stream-state #f)
  (let ((tc (car (gemini-stream-state-blocks stream-state))))
    (check (pi-tool-call-block-id tc) => "bash_0_1")
    (check (pi-tool-call-block-name tc) => "bash")
  ) ;let
) ;let*

;; 回调为 #f 时不派发事件，但状态仍然更新
(let ((stream-state (vector '() #f '() '() "STOP" #f "" "")))
  (process-stream-function-call '(("name" . "bash") ("args" . ()) ("id" . "call_x"))
    '()
    stream-state
    #f
  ) ;process-stream-function-call
  (check (length (gemini-stream-state-blocks stream-state)) => 1)
) ;let*

(check-report)
