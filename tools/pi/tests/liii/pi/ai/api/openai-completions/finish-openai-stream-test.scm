;; finish-openai-stream
;; 完成 OpenAI 流式请求，将累积的工具调用转换为 toolCall 块写入 blocks。
;;
;; 语法
;; ----
;; (finish-openai-stream stream-state on-event)
;;
;; 参数
;; ----
;; stream-state : openai-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; on-event : procedure | #f
;; 接收流式事件的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用将工具调用块追加到 blocks，并清空 tools 表。

(import (liii check)
  (liii pi ai message)
  (liii pi ai api openai-completions)
  (liii pi ai api openai-completions stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 包含工具调用时完成并清空 tools 表
(let* ((stream-state (make-openai-stream-state '() #f '() "stop" #f '()))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
       (tc '(("index" . 0)
             ("id" . "call_1")
             ("function" . (("name" . "bash") ("arguments" . "{\"command\":\"ls\"}")))))
      ) ;
  (process-openai-tool-call-delta tc stream-state emit)
  (set! events '())
  (finish-openai-stream stream-state emit)
  (check (length (openai-stream-state-blocks stream-state)) => 1)
  (let ((tc-blk (car (openai-stream-state-blocks stream-state))))
    (check (pi-tool-call-block? tc-blk) => #t)
    (check (pi-tool-call-block-name tc-blk) => "bash")
    (check (cdr (assoc "command" (pi-tool-call-block-arguments tc-blk))) => "ls")
  ) ;let
  (check (openai-stream-state-stop-reason stream-state) => "toolUse")
  (check (length (openai-stream-state-tools stream-state)) => 0)
  (check (length events) => 1)
  (check (cdr (assoc 'type (car events))) => "toolcall_end")
) ;let*

;; 无工具调用时无操作
(let ((stream-state (make-openai-stream-state '() #f '() "stop" #f '())))
  (finish-openai-stream stream-state #f)
  (check (length (openai-stream-state-blocks stream-state)) => 0)
) ;let*

(check-report)
