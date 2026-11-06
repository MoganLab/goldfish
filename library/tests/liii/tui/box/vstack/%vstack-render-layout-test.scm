;; %vstack-render-layout
;; 渲染子组件并量出各子组件的行高（子组件仅渲染一次），用于鼠标命中。
;;
;; 语法
;; ----
;; (%vstack-render-layout children spacing width)
;;
;; 参数
;; ----
;; children : list?
;; 子组件列表。
;;
;; spacing : integer?
;; 间距行数。
;;
;; width : integer?
;; 渲染宽度。
;;
;; 返回值
;; ----
;; pair?
;; 点对 (lines . heights)，lines 为渲染行列表，heights 为各组件行高列表。

(import (scheme base) (liii check) (liii tui component) (liii tui box vstack))

(check-set-mode! 'report-failed)

(let* ((c1 (make-text "one\ntwo"))
       (c2 (make-text "three"))
       (res (%vstack-render-layout (list c1 c2) 1 20))
      ) ;
  (check (car res) => '("one" "two" "" "three"))
  (check (cdr res) => '(2 1))
) ;let*

(check-report)
