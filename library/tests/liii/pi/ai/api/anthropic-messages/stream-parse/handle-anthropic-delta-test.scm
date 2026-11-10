;; handle-anthropic-delta
;; 处理 Anthropic content_block_delta 事件，按增量类型分发到文本、工具参数或 thinking 处理逻辑。
;;
;; 语法
;; ----
;; (handle-anthropic-delta json-obj stream-state on-ev)
;;
;; 参数
;; ----
;; json-obj : alist
;; content_block_delta 事件对象，包含 index 与 delta 字段。
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
;; text_delta 追加文本；input_json_delta 拼接工具参数 JSON 片段；
;; thinking_delta 与 signature_delta 累积思考内容。

(import (liii check)
  (liii pi ai api anthropic-messages core)
  (liii pi ai api anthropic-messages stream-parse)
  (liii pi ai api anthropic-messages stream-state)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; text_delta 追加文本并派发 text_delta 事件
(let* ((evs '())
       (on-ev (lambda (e) (set! evs (cons e evs))))
       (stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
      ) ;
  (handle-anthropic-delta '(("type" . "content_block_delta")
                            ("index" . 0)
                            ("delta" . (("type" . "text_delta") ("text" . "Hi"))))
    stream-state
    on-ev
  ) ;handle-anthropic-delta
  (check (anthropic-stream-state-last-text stream-state) => "Hi")
  (check (length evs) => 2)
  (check (cdr (assoc 'type (car evs))) => "text_delta")
  (check (cdr (assoc 'delta (car evs))) => "Hi")
) ;let*

;; input_json_delta 拼接工具参数 JSON 片段
(let* ((evs '())
       (on-ev (lambda (e) (set! evs (cons e evs))))
       (stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
      ) ;
  ;; 先建立工具块状态
  (process-anthropic-sse '(("type" . "content_block_start")
                           ("index" . 1)
                           ("content_block" . (("type" . "tool_use")
                                               ("id" . "call_1")
                                               ("name" . "bash"))))
    stream-state
    on-ev
  ) ;process-anthropic-sse
  (set! evs '())
  (handle-anthropic-delta '(("type" . "content_block_delta")
                            ("index" . 1)
                            ("delta" . (("type" . "input_json_delta")
                                        ("partial_json" . "{\"command\":"))))
    stream-state
    on-ev
  ) ;handle-anthropic-delta
  (check (length evs) => 1)
  (check (cdr (assoc 'type (car evs))) => "toolcall_delta")
  (check (cdr (assoc 'delta (car evs))) => "{\"command\":")
) ;let*

;; thinking_delta 累积思考内容
(let* ((evs '())
       (on-ev (lambda (e) (set! evs (cons e evs))))
       (stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
      ) ;
  ;; 先建立 thinking 块状态
  (process-anthropic-sse '(("type" . "content_block_start")
                           ("index" . 2)
                           ("content_block" . (("type" . "thinking"))))
    stream-state
    on-ev
  ) ;process-anthropic-sse
  (set! evs '())
  (handle-anthropic-delta '(("type" . "content_block_delta")
                            ("index" . 2)
                            ("delta" . (("type" . "thinking_delta")
                                        ("thinking" . "thinking..."))))
    stream-state
    on-ev
  ) ;handle-anthropic-delta
  (check (length evs) => 1)
  (check (cdr (assoc 'type (car evs))) => "thinking_delta")
  (check (cdr (assoc 'delta (car evs))) => "thinking...")
) ;let*

;; 未知增量类型被忽略
(let* ((stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
      ) ;
  (handle-anthropic-delta '(("type" . "content_block_delta")
                            ("index" . 0)
                            ("delta" . (("type" . "unknown_delta"))))
    stream-state
    #f
  ) ;handle-anthropic-delta
  (check (anthropic-stream-state-last-text stream-state) => #f)
) ;let*

(check-report)
