;; stop-anthropic-current-block
;; 结束 Anthropic 流式当前内容块，把工具调用或文本块写入消息块列表。
;;
;; 语法
;; ----
;; (stop-anthropic-current-block stream-state on-ev)
;;
;; 参数
;; ----
;; stream-state : anthropic-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; on-ev : procedure | #f
;; 接收 toolcall_end 或 text_end 事件的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用追加内容块并清除当前块状态。
;;
;; 说明
;; ----
;; 当前是工具块时解析参数 JSON 并生成 toolCall 块；
;; 当前是 thinking 块时结束 thinking 块；当前是文本块时派发 text_end 并清除 last-text。

(import (liii check)
  (liii pi ai api anthropic-messages)
  (liii pi ai api anthropic-messages stream-state)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; 工具块结束时解析参数并写入 toolCall 块
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
  (handle-anthropic-delta '(("type" . "content_block_delta")
                            ("index" . 0)
                            ("delta" . (("type" . "input_json_delta")
                                        ("partial_json" . "{\"command\":\"ls\"}"))))
    stream-state
    on-ev
  ) ;handle-anthropic-delta
  (set! evs '())
  (stop-anthropic-current-block stream-state on-ev)
  (check (length (anthropic-stream-state-blocks stream-state)) => 1)
  (let ((tc (car (anthropic-stream-state-blocks stream-state))))
    (check (pi-tool-call-block? tc) => #t)
    (check (pi-tool-call-block-id tc) => "call_1")
    (check (pi-tool-call-block-name tc) => "bash")
    (check (cdr (assoc "command" (pi-tool-call-block-arguments tc))) => "ls"))
  (check (length (anthropic-stream-state-tool-calls stream-state)) => 1)
  (check (length evs) => 1)
  (check (cdr (assoc 'type (car evs))) => "toolcall_end")
  (check (cdr (assoc 'id (car evs))) => "call_1")
) ;let*

;; 空工具参数归一化为空 alist
(let* ((stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
      ) ;
  (process-anthropic-sse '(("type" . "content_block_start")
                           ("index" . 0)
                           ("content_block" . (("type" . "tool_use")
                                               ("id" . "call_empty")
                                               ("name" . "ls"))))
    stream-state
    #f
  ) ;process-anthropic-sse
  (stop-anthropic-current-block stream-state #f)
  (check (length (anthropic-stream-state-blocks stream-state)) => 1)
  (check (pi-tool-call-block-arguments (car (anthropic-stream-state-blocks stream-state)))
    => '())
) ;let*

;; 文本块结束时派发 text_end 并清除 last-text
(let* ((evs '())
       (on-ev (lambda (e) (set! evs (cons e evs))))
       (stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
      ) ;
  (append-anthropic-stream-text stream-state "done" on-ev)
  (set! evs '())
  (stop-anthropic-current-block stream-state on-ev)
  (check (anthropic-stream-state-last-text stream-state) => #f)
  (check (length evs) => 1)
  (check (cdr (assoc 'type (car evs))) => "text_end")
  (check (cdr (assoc 'content (car evs))) => "done")
) ;let*

;; 没有活动块时不产生事件
(let* ((evs '())
       (on-ev (lambda (e) (set! evs (cons e evs))))
       (stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
      ) ;
  (stop-anthropic-current-block stream-state on-ev)
  (check (length evs) => 0)
  (check (length (anthropic-stream-state-blocks stream-state)) => 0)
) ;let*

(check-report)
