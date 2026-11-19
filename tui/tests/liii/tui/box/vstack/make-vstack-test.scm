;; make-vstack
;; 创建垂直堆叠排版组件，将多个子组件纵向依次拼接排版。
;;
;; 语法
;; ----
;; (make-vstack children [:spacing spacing])
;; (make-vstack child1 child2 ...)
;;
;; 参数
;; ----
;; children : list? 或 离散参数
;; 子组件列表（字符串自动包装为 make-text）。
;;
;; spacing : integer? (可选，默认为 0)
;; 相邻子组件之间的纵向空行间距。
;;
;; 返回值
;; ----
;; component?
;; 垂直堆叠组件实例。

(import (scheme base)
  (liii check)
  (liii tui text-width)
  (liii tui component)
  (liii tui box)
) ;import

(check-set-mode! 'report-failed)

;; 1. 基础创建与 Component 协议检查
(let ((v (make-vstack (list (make-text "line 1") (make-text "line 2")))))
  (check (component? v) => #t)
  (check (vstack? v) => #t)
) ;let

;; 支持不定参数（可直接传入子组件列表，或以多个位置参数传入）
(let ((v (make-vstack "line 1" "line 2")))
  (check (component? v) => #t)
  (check (vstack? v) => #t)
) ;let

;; 2. 垂直排列渲染
(let* ((v (make-vstack "first" "second")) (lines (component-render v 20)))
  (check (length lines) => 2)
  (check (car lines) => "first")
  (check (cadr lines) => "second")
) ;let*

;; 3. 间距 spacing 支持
(let* ((v (make-vstack (list "first" "second") :spacing 1))
       (lines (component-render v 20))
      ) ;
  (check (length lines) => 3)
  (check (car lines) => "first")
  (check (cadr lines) => "")
  (check (caddr lines) => "second")
) ;let*

(check-report)
