;; make-queued-card
;; 构造并返回新的 make-queued-card 实例组件。
;;
;; 语法
;; ----
;; (make-queued-card ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; component?
;; 返回构造的实例。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

;; 等待队列提示卡片使用 dim 前景
(let* ((queued-card (make-queued-card "等待执行的后续指令"))
       (w 40)
       (lines (component-render queued-card w))
      ) ;
  (check (component? queued-card) => #t)
  (check (string-contains? (car lines) "Queued") => #t)
  (check (string-contains? (car lines) "等待执行的后续指令") => #t)
  (check (string-contains? (car lines) "38;2;126;136;142") => #t)
  (check (eq? lines (component-render queued-card w)) => #t)
  (check-false (eq? lines (component-render queued-card 20)))
) ;let*

(check-report)
