;; gemini-finalize-stream-success
;; 结束 Gemini 流式请求并生成成功的 assistant 消息。
;;
;; 语法
;; ----
;; (gemini-finalize-stream-success stream-state on-event)
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
;; pi-assistant-message
;; 包含 content、stopReason、usage 与可选 errorMessage 的消息 record。
;;
;; 说明
;; ----
;; 先结束当前活动块，再按 finishReason 与是否存在函数调用映射 stopReason；
;; finishReason 缺省时按 STOP 处理。

(import (liii check)
  (liii pi ai event)
  (liii pi ai message)
  (liii pi ai api google-generative-ai stream-result)
  (liii pi ai api google-generative-ai stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 普通结束：stopReason 为 stop
(let ((stream-state (make-gemini-stream-state '() #f '((input . 1) (output . 2)) '() "STOP" #f "" "")))
  (let ((msg (gemini-finalize-stream-success stream-state #f)))
    (check (cdr (assoc 'role msg)) => "assistant")
    (check (cdr (assoc 'stopReason msg)) => "stop")
    (check (cdr (assoc 'input (cdr (assoc 'usage msg)))) => 1)
  ) ;let
) ;let*

;; 存在工具调用时 stopReason 为 toolUse
(let ((stream-state (make-gemini-stream-state '() #f '() '(tc) "STOP" #f "" "")))
  (let ((msg (gemini-finalize-stream-success stream-state #f)))
    (check (cdr (assoc 'stopReason msg)) => "toolUse")
  ) ;let
) ;let*

;; MAX_TOKENS 截断
(let ((stream-state (make-gemini-stream-state '() #f '() '() "MAX_TOKENS" #f "" "")))
  (let ((msg (gemini-finalize-stream-success stream-state #f)))
    (check (cdr (assoc 'stopReason msg)) => "length")
  ) ;let
) ;let*

;; 未知 finishReason 映射为 error 并带错误信息
(let ((stream-state (make-gemini-stream-state '() #f '() '() "SAFETY" #f "" "")))
  (let ((msg (gemini-finalize-stream-success stream-state #f)))
    (check (cdr (assoc 'stopReason msg)) => "error")
    (check (cdr (assoc 'errorMessage msg)) => "SAFETY")
  ) ;let
) ;let*

;; 缺少 finishReason 时按 STOP 处理
(let ((stream-state (make-gemini-stream-state '() #f '() '() #f #f "" "")))
  (let ((msg (gemini-finalize-stream-success stream-state #f)))
    (check (cdr (assoc 'stopReason msg)) => "stop")
  ) ;let
) ;let*

(check-report)
