;; append-openai-stream-text
;; 向 OpenAI 流状态追加文本增量，并派发 text_start / text_delta 事件。
;;
;; 语法
;; ----
;; (append-openai-stream-text stream-state text on-event)
;;
;; 参数
;; ----
;; stream-state : openai-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; text : string
;; 本次到达的文本增量。
;; on-event : procedure | #f
;; 接收流式事件的回调；为 #f 时不派发事件。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用追加文本块并更新 last-text。
;;
;; 说明
;; ----
;; 非字符串或空字符串被直接忽略；首个增量派发 text_start 与 text_delta，
;; 后续增量只派发 text_delta。

(import (liii check)
  (liii pi ai message)
  (liii pi ai api openai-completions)
  (liii pi ai api openai-completions stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 首个增量新建文本块
(let* ((stream-state (make-openai-stream-state '() #f '((input . 1)) "stop" #f '()))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
      ) ;
  (append-openai-stream-text stream-state "Hello" emit)
  (check (length (openai-stream-state-blocks stream-state)) => 1)
  (check (pi-text-block-text (car (openai-stream-state-blocks stream-state))) => "Hello")
  (check (openai-stream-state-last-text stream-state) => "Hello")
  (check (length events) => 2)
  (check (cdr (assoc 'type (car events))) => "text_start")
  (check (cdr (assoc 'type (cadr events))) => "text_delta")
) ;let*

;; 后续增量合并到现有文本块
(let* ((stream-state (make-openai-stream-state '() #f '() "stop" #f '()))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
      ) ;
  (append-openai-stream-text stream-state "Hello" emit)
  (set! events '())
  (append-openai-stream-text stream-state " world" emit)
  (check (length (openai-stream-state-blocks stream-state)) => 1)
  (check (pi-text-block-text (car (openai-stream-state-blocks stream-state))) => "Hello world")
  (check (openai-stream-state-last-text stream-state) => "Hello world")
  (check (length events) => 1)
  (check (cdr (assoc 'type (car events))) => "text_delta")
) ;let*

;; 空文本被忽略
(let* ((stream-state (make-openai-stream-state '() #f '() "stop" #f '()))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
      ) ;
  (append-openai-stream-text stream-state "" emit)
  (append-openai-stream-text stream-state #f emit)
  (check (length (openai-stream-state-blocks stream-state)) => 0)
  (check (length events) => 0)
) ;let*

(check-report)
