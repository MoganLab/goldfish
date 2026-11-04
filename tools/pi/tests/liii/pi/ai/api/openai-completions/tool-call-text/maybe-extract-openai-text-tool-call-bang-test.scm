;; maybe-extract-openai-text-tool-call!
;; 流结束时从正文里补齐内联工具调用，或把思考内容转成普通文本块。
;;
;; 语法
;; ----
;; (maybe-extract-openai-text-tool-call! stream-state on-event)
;;
;; 参数
;; ----
;; stream-state : openai-stream-state
;; 可变流状态。
;; on-event : procedure | #f
;; 接收流式事件的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用写入 blocks 并派发 toolcall_start / toolcall_end 事件。

(import (liii check)
  (liii pi ai message)
  (liii pi ai api openai-completions stream-state)
  (liii pi ai api openai-completions tool-call-text)
) ;import

(check-set-mode! 'report-failed)

;; 正文里含工具调用：写入 toolCall 块并派发事件
(let* ((stream-state
         (make-openai-stream-state '()
           "<tool_call><function=bash><parameter=command>ls</parameter></function></tool_call>"
           '() "stop" #f '()))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
      ) ;
  (maybe-extract-openai-text-tool-call! stream-state emit)
  (check (length (openai-stream-state-blocks stream-state)) => 1)
  (let ((tc-blk (car (openai-stream-state-blocks stream-state))))
    (check (pi-tool-call-block? tc-blk) => #t)
    (check (pi-tool-call-block-name tc-blk) => "bash")
    (check (cdr (assoc "command" (pi-tool-call-block-arguments tc-blk))) => "ls")
  ) ;let
  (check (openai-stream-state-stop-reason stream-state) => "toolUse")
  (check (length events) => 2)
  (check (cdr (assoc 'type (car events))) => "toolcall_start")
  (check (cdr (assoc 'type (cadr events))) => "toolcall_end")
) ;let*

;; 只有思考内容：转成普通文本块
(let ((stream-state
        (make-openai-stream-state '() #f '() "stop" #f '() "  thinking hard  ")))
  (maybe-extract-openai-text-tool-call! stream-state #f)
  (check (length (openai-stream-state-blocks stream-state)) => 1)
  (check (pi-text-block? (car (openai-stream-state-blocks stream-state))) => #t)
  (check (openai-stream-state-last-text stream-state) => "thinking hard")
) ;let*

;; 既没有工具调用也没有思考内容：无操作
(let ((stream-state (make-openai-stream-state '() #f '() "stop" #f '())))
  (maybe-extract-openai-text-tool-call! stream-state #f)
  (check (length (openai-stream-state-blocks stream-state)) => 0)
) ;let*

(check-report)
