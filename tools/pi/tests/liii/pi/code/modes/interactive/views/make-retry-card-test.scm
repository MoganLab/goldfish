;; make-retry-card
;; 构造并返回新的 make-retry-card 实例组件。
;;
;; 语法
;; ----
;; (make-retry-card ...)
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

(let ((plain
        (string-join (map strip-ansi (component-render (make-retry-card "retry\x1b;[?1000lnow") 80))
          "\n"
        ) ;string-join
      ) ;plain
     ) ;
  (check-true (string-contains? plain "retrynow"))
  (check-false (string-contains? plain "1000l"))
) ;let

(check-report)
