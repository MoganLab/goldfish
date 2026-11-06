;; %vstack-render
;; 纯函数垂直渲染：渲染各子组件并在相邻组件间按间距插入空行。
;;
;; 语法
;; ----
;; (%vstack-render children spacing width)
;;
;; 参数
;; ----
;; children : list?
;; 子组件列表。
;;
;; spacing : integer?
;; 纵向间距空行数。
;;
;; width : integer?
;; 可用渲染宽度。
;;
;; 返回值
;; ----
;; list?
;; 拼接好的文本行列表。

(import (scheme base) (liii check) (liii tui component) (liii tui box vstack))

(check-set-mode! 'report-failed)

(let ((c1 (make-text "a")) (c2 (make-text "b")))
  (check (%vstack-render (list c1 c2) 1 20) => '("a" "" "b"))
  (check (%vstack-render (list c1 c2) 0 20) => '("a" "b"))
  (check (%vstack-render '() 0 20) => '())
) ;let

(check-report)
