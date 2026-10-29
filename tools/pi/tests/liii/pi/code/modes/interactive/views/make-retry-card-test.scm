(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

;; 重试提示卡片 pending 背景整行填充
(let* ((retry-card (make-retry-card "[pi retry] HTTP 429, attempt 1/12, waiting 2.0s"))
       (w 60)
       (lines (component-render retry-card w))
      ) ;
  (check (component? retry-card) => #t)
  (check (pair? lines) => #t)
  (check (string-contains? (car lines) "↻") => #t)
  (check (string-contains? (car lines) "HTTP 429") => #t)
  (check (string-contains? (car lines) "\x1b;[48;2;") => #t)
  (check (eq? lines (component-render retry-card w)) => #t)
  (check-false (eq? lines (component-render retry-card 30)))
) ;let*

(check-report)
