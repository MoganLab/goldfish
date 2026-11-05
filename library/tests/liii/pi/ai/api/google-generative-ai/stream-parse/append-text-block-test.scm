;; append-text-block
;; 把文本增量追加到 Gemini 流状态的消息块列表，并派发对应事件。
;;
;; 语法
;; ----
;; (append-text-block blocks last-text t cur-idx usage stream-state on-event)
;;
;; 参数
;; ----
;; blocks : list
;; 当前已完成的消息块列表。
;; last-text : string | #f
;; 当前正在累积的文本；为 #f 表示需要新建文本块。
;; t : string
;; 本次到达的文本增量。
;; cur-idx : integer
;; 当前内容块索引。
;; usage : alist
;; 当前用量统计。
;; stream-state : gemini-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; on-event : procedure | #f
;; 接收流式事件的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; boolean | unspecified
;; 空文本被忽略时返回 #t；其余情况通过副作用更新流状态。

(import (liii check)
  (liii pi ai message)
  (liii pi ai api google-generative-ai stream-parse)
  (liii pi ai api google-generative-ai stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 首个增量新建文本块并派发 text_start 与 text_delta
(let* ((stream-state (vector '() #f '() '() "STOP" #f "" ""))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
      ) ;
  (append-text-block '() #f "hello" 0 '((input . 1)) stream-state emit)
  (check (length (gemini-stream-state-blocks stream-state)) => 1)
  (check (pi-text-block-text (car (gemini-stream-state-blocks stream-state))) => "hello")
  (check (gemini-stream-state-last-text stream-state) => "hello")
  (check (length events) => 2)
  (check (cdr (assoc 'type (car events))) => "text_start")
  (check (cdr (assoc 'type (cadr events))) => "text_delta")
) ;let*

;; 后续增量合并到最后一个文本块，只派发 text_delta
(let* ((stream-state (vector '() #f '() '() "STOP" #f "" ""))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
      ) ;
  (append-text-block '() #f "hello" 0 '() stream-state emit)
  (set! events '())
  (append-text-block (gemini-stream-state-blocks stream-state)
    (gemini-stream-state-last-text stream-state)
    " world"
    0
    '()
    stream-state
    emit
  ) ;append-text-block
  (check (length (gemini-stream-state-blocks stream-state)) => 1)
  (check (pi-text-block-text (car (gemini-stream-state-blocks stream-state))) => "hello world")
  (check (length events) => 1)
  (check (cdr (assoc 'type (car events))) => "text_delta")
) ;let*

;; 空文本被忽略
(let* ((stream-state (vector '() #f '() '() "STOP" #f "" ""))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
      ) ;
  (check (append-text-block '() #f "" 0 '() stream-state emit) => #t)
  (check (length (gemini-stream-state-blocks stream-state)) => 0)
  (check (length events) => 0)
) ;let*

(check-report)
