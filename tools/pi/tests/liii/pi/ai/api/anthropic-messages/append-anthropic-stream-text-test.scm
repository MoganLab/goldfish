;; append-anthropic-stream-text
;; 将 Anthropic 流式文本增量追加到当前文本块，并派发 text_start / text_delta 事件。
;;
;; 语法
;; ----
;; (append-anthropic-stream-text stream-state t on-ev)
;;
;; 参数
;; ----
;; stream-state : anthropic-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; t : string
;; 本次到达的文本增量。
;; on-ev : procedure | #f
;; 接收 text_start 与 text_delta 事件的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用追加文本块并更新 last-text。
;;
;; 说明
;; ----
;; 首个增量会新建文本块，后续增量合并到最后一个文本块；
;; 文本块开始时先派发一次 text_start 事件。

(import (liii check)
  (liii pi ai api anthropic-messages)
  (liii pi ai api anthropic-messages stream-state)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; 首个增量新建文本块并派发 text_start + text_delta
(let* ((evs '())
       (on-ev (lambda (e) (set! evs (cons e evs))))
       (stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
      ) ;
  (append-anthropic-stream-text stream-state "Hello" on-ev)
  (check (length (anthropic-stream-state-blocks stream-state)) => 1)
  (check (pi-text-block-text (car (anthropic-stream-state-blocks stream-state)))
    => "Hello")
  (check (anthropic-stream-state-last-text stream-state) => "Hello")
  (check (length evs) => 2)
  (check (cdr (assoc 'type (cadr evs))) => "text_start")
  (check (cdr (assoc 'type (car evs))) => "text_delta")
  (check (cdr (assoc 'delta (car evs))) => "Hello")
  (check (cdr (assoc 'contentIndex (car evs))) => 0)
) ;let*

;; 后续增量合并到最后一个文本块，不再派发 text_start
(let* ((evs '())
       (on-ev (lambda (e) (set! evs (cons e evs))))
       (stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
      ) ;
  (append-anthropic-stream-text stream-state "Hello" on-ev)
  (set! evs '())
  (append-anthropic-stream-text stream-state " Anthropic" on-ev)
  (check (length (anthropic-stream-state-blocks stream-state)) => 1)
  (check (pi-text-block-text (car (anthropic-stream-state-blocks stream-state)))
    => "Hello Anthropic")
  (check (anthropic-stream-state-last-text stream-state) => "Hello Anthropic")
  (check (length evs) => 1)
  (check (cdr (assoc 'type (car evs))) => "text_delta")
) ;let*

;; 已有其它块时新增文本块，索引指向新块
(let* ((stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f))
      ) ;
  (anthropic-stream-state-blocks-set! stream-state
    (list (pi-text-block "prev")))
  (anthropic-stream-state-last-text-set! stream-state #f)
  (append-anthropic-stream-text stream-state "next" #f)
  (check (length (anthropic-stream-state-blocks stream-state)) => 2)
  (check (pi-text-block-text (cadr (anthropic-stream-state-blocks stream-state)))
    => "next")
) ;let*

;; 回调为 #f 时不派发事件，但状态仍然更新
(let ((stream-state (vector '() #f (anthropic-map-usage '()) '() "end_turn" #f #f)))
  (append-anthropic-stream-text stream-state "quiet" #f)
  (check (anthropic-stream-state-last-text stream-state) => "quiet")
) ;let*

(check-report)
