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
  (liii list)
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
  (check-false (string-contains? (car lines) "Queued"))
  (check-false (string-contains? (strip-ansi (car lines)) "["))
  (check-false (string-contains? (strip-ansi (car lines)) "]"))
  (check (string-contains? (car lines) "等待执行的后续指令") => #t)
  (check (string-contains? (car lines) "38;2;126;136;142") => #t)
  (check (map strip-ansi lines) => '("  等待执行的后续指令"))
  (check (eq? lines (component-render queued-card w)) => #t)
  (check-false (eq? lines (component-render queued-card 20)))
) ;let*

(check (map strip-ansi (component-render (make-queued-card "before\x1b;cafter") 80))
  =>
  '("  beforeafter")
) ;check

;; 超长单行文本按宽度折行，每行显示宽度不超过 width
(let* ((long-line (make-string 120 #\a))
       (lines (component-render (make-queued-card long-line) 40))
      ) ;
  (check-true (> (length lines) 1))
  (check-true (every (lambda (l) (<= (display-width (strip-ansi l)) 40)) lines))
) ;let*

;; 超长多行日志限制最大预览行数，避免撑爆终端高度
(let* ((many-lines (string-join (map (lambda (i) (string-append "log line " (number->string i))) (iota 200)) "\n"))
       (lines (component-render (make-queued-card many-lines) 80))
      ) ;
  (check-true (<= (length lines) 8))
  (check-true (every (lambda (l) (<= (display-width (strip-ansi l)) 80)) lines))
) ;let*

;; \r\n 与 \r 被规范化，不会在渲染行中残留 \r
(let ((lines (map strip-ansi (component-render (make-queued-card "a\r\nb\rc") 80))))
  (check lines => '("  a" "  b" "  c"))
) ;let

(check-report)
