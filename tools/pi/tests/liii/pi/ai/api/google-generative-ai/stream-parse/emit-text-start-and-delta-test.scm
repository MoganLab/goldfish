;; emit-text-start-and-delta
;; 派发 text_start 与 text_delta 事件，标记文本块开始并推送增量。
;;
;; 语法
;; ----
;; (emit-text-start-and-delta on-event cur-idx text usage)
;;
;; 参数
;; ----
;; on-event : procedure | #f
;; 接收流式事件的回调；为 #f 时不派发事件。
;; cur-idx : integer
;; 当前内容块索引。
;; text : string
;; 本次到达的文本增量。
;; usage : alist
;; 当前用量统计。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用派发 text_start 与 text_delta 事件。

(import (liii check)
  (liii pi ai api google-generative-ai stream-parse)
) ;import

(check-set-mode! 'report-failed)

;; 派发两个事件
(let* ((events '())
       (emit (lambda (ev) (set! events (append events (list ev)))))
      ) ;
  (emit-text-start-and-delta emit 0 "hello" '((input . 1)))
  (check (length events) => 2)
  (check (cdr (assoc 'type (car events))) => "text_start")
  (check (cdr (assoc 'contentIndex (car events))) => 0)
  (check (cdr (assoc 'usage (car events))) => '((input . 1)))
  (check (cdr (assoc 'type (cadr events))) => "text_delta")
  (check (cdr (assoc 'delta (cadr events))) => "hello")
  (check (cdr (assoc 'contentIndex (cadr events))) => 0)
) ;let*

;; 回调为 #f 时不派发事件
(let ((events '()))
  (emit-text-start-and-delta #f 0 "hello" '())
  (check (length events) => 0)
) ;let*

(check-report)
