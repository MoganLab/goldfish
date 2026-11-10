;; make-hstack
;; 创建水平分列排版组件，将多个子组件横向并列排版。
;;
;; 语法
;; ----
;; (make-hstack children [:spacing spacing] [:widths widths])
;; (make-hstack child1 child2 ...)
;;
;; 参数
;; ----
;; children : list? 或 离散参数
;; 子组件列表（字符串自动包装为 make-text）。
;;
;; spacing : integer? (可选，默认为 0)
;; 列间距。
;;
;; widths : symbol? 或 list? (可选，默认为 'auto)
;; 列宽规格配置。
;;
;; 返回值
;; ----
;; component?
;; 水平分列组件实例。

(import (scheme base)
  (liii check)
  (liii tui text-width)
  (liii tui component)
  (liii tui box)
) ;import

(check-set-mode! 'report-failed)

;; 1. 基础创建与 Component 协议检查
(let ((h (make-hstack (list "left" "right"))))
  (check (component? h) => #t)
  (check (hstack? h) => #t)
) ;let

(let ((h (make-hstack "col1" "col2")))
  (check (component? h) => #t)
  (check (hstack? h) => #t)
) ;let

;; 2. 默认均分宽度并排渲染
(let* ((h (make-hstack "left" "right")) (lines (component-render h 20)))
  (check (length lines) => 1)
  (check (display-width (car lines)) => 20)
  (check (car lines) => "left      right     ")
) ;let*

;; 3. 列间距 spacing 支持
(let* ((h (make-hstack (list "left" "right") :spacing 2))
       (lines (component-render h 20))
      ) ;
  (check (length lines) => 1)
  (check (display-width (car lines)) => 20)
  (check (car lines) => "left       right    ")
) ;let*

(check-report)
