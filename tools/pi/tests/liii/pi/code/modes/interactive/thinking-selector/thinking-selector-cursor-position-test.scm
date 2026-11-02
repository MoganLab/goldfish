;; thinking-selector-cursor-position
;; 获取思维链选择器当前光标行与列位置列表 (list row col)。
;;
;; 语法
;; ----
;; (thinking-selector-cursor-position comp)
;;
;; 参数
;; ----
;; comp : component?
;; 思维链选择器组件实例。
;;
;; 返回值
;; ----
;; list?
;; 包含光标行列相对位置的列表。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive thinking-selector)
) ;import

(check-set-mode! 'report-failed)

(let* ((ts (make-thinking-selector "off"
             '("off" "low" "high")
             "off"
             (lambda (l) #t)
             (lambda () #t)
             (lambda (l) #t)
           ) ;make-thinking-selector
       ) ;ts
       (pos (thinking-selector-cursor-position ts))
      ) ;
  (check-true (pair? pos))
  (check (number? (car pos)) => #t)
  (check (number? (cadr pos)) => #t)
) ;let*

(check-report)
