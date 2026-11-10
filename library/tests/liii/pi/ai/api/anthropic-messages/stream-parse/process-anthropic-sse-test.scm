;; process-anthropic-sse
;; 处理 Anthropic SSE 事件对象，按事件类型更新流状态并派发内部事件。
;;
;; 语法
;; ----
;; (process-anthropic-sse json-obj stream-state on-ev)
;;
;; 参数
;; ----
;; json-obj : alist
;; 已解析的 Anthropic SSE 事件对象。
;; stream-state : anthropic-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; on-ev : procedure | #f
;; 接收 text、toolcall 与 thinking 事件 alist 的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态并派发对应事件。
;;
;; 说明
;; ----
;; message_start 初始化 usage；content_block_start 建立工具或 thinking 块；
;; content_block_delta 追加增量；content_block_stop 结束当前块；
;; message_delta 更新 stop_reason 与 usage；error 事件把 stop_reason 置为 error。

(import (liii check)
  (liii pi ai api anthropic-messages core)
  (liii pi ai api anthropic-messages stream-parse)
  (liii pi ai api anthropic-messages stream-state)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; message_start 初始化 usage
(let ((stream-state (vector '() #f '() '() "end_turn" #f #f)))
  (process-anthropic-sse '(("type" . "message_start")
                           ("message" . (("usage" . (("input_tokens" . 11)
                                                     ("output_tokens" . 3))))))
    stream-state
    #f
  ) ;process-anthropic-sse
  (check (cdr (assoc 'input (anthropic-stream-state-usage stream-state))) => 11)
  (check (cdr (assoc 'output (anthropic-stream-state-usage stream-state))) => 3)
) ;let*

;; content_block_start 建立工具块并派发 toolcall_start
(let* ((evs '())
       (on-ev (lambda (e) (set! evs (cons e evs))))
       (stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
      ) ;
  (process-anthropic-sse '(("type" . "content_block_start")
                           ("index" . 0)
                           ("content_block" . (("type" . "tool_use")
                                               ("id" . "call_1")
                                               ("name" . "bash"))))
    stream-state
    on-ev
  ) ;process-anthropic-sse
  (check (length evs) => 1)
  (check (cdr (assoc 'type (car evs))) => "toolcall_start")
  (check (cdr (assoc 'id (car evs))) => "call_1")
  (check (cdr (assoc 'toolName (car evs))) => "bash")
) ;let*

;; content_block_start 建立 thinking 块并派发 thinking_start
(let* ((evs '())
       (on-ev (lambda (e) (set! evs (cons e evs))))
       (stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
      ) ;
  (process-anthropic-sse '(("type" . "content_block_start")
                           ("index" . 0)
                           ("content_block" . (("type" . "thinking"))))
    stream-state
    on-ev
  ) ;process-anthropic-sse
  (check (length evs) => 1)
  (check (cdr (assoc 'type (car evs))) => "thinking_start")
) ;let*

;; content_block_delta 追加文本增量
(let ((stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f)))
  (process-anthropic-sse '(("type" . "content_block_delta")
                           ("index" . 0)
                           ("delta" . (("type" . "text_delta") ("text" . "hey"))))
    stream-state
    #f
  ) ;process-anthropic-sse
  (check (anthropic-stream-state-last-text stream-state) => "hey")
) ;let*

;; content_block_stop 结束当前文本块
(let ((stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f)))
  (process-anthropic-sse '(("type" . "content_block_delta")
                           ("index" . 0)
                           ("delta" . (("type" . "text_delta") ("text" . "hey"))))
    stream-state
    #f
  ) ;process-anthropic-sse
  (process-anthropic-sse '(("type" . "content_block_stop") ("index" . 0))
    stream-state
    #f
  ) ;process-anthropic-sse
  (check (anthropic-stream-state-last-text stream-state) => #f)
) ;let*

;; message_delta 更新 stop_reason 与 usage
(let ((stream-state (vector '() #f '() '() "end_turn" #f #f)))
  (process-anthropic-sse '(("type" . "message_delta")
                           ("delta" . (("stop_reason" . "tool_use")))
                           ("usage" . (("output_tokens" . 42))))
    stream-state
    #f
  ) ;process-anthropic-sse
  (check (anthropic-stream-state-stop-reason stream-state) => "tool_use")
  (check (cdr (assoc 'output (anthropic-stream-state-usage stream-state))) => 42)
) ;let*

;; error 事件把 stop_reason 置为 error
(let ((stream-state (vector '() #f '() '() "end_turn" #f #f)))
  (process-anthropic-sse '(("type" . "error"))
    stream-state
    #f
  ) ;process-anthropic-sse
  (check (anthropic-stream-state-stop-reason stream-state) => "error")
) ;let*

;; 未知事件类型被忽略
(let ((stream-state (vector '() #f '() '() "end_turn" #f #f)))
  (process-anthropic-sse '(("type" . "ping"))
    stream-state
    #f
  ) ;process-anthropic-sse
  (check (anthropic-stream-state-stop-reason stream-state) => "end_turn")
) ;let*

(check-report)
