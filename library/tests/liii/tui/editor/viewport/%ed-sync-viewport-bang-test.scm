;; %ed-sync-viewport!
;; 同步编辑器的滚动视口偏移量，确保光标始终保持在可视窗口内。
;;
;; 语法
;; ----
;; (%ed-sync-viewport! data width)
;;
;; 参数
;; ----
;; data : any
;; 编辑器内部数据。
;;
;; width : integer?
;; 编辑器渲染宽度。
;;
;; 返回值
;; ----
;; integer?
;; 计算更新后的顶部起始行索引（top-row）。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui editor component)
  (liii tui editor viewport)
) ;import

(check-set-mode! 'report-failed)

(let* ((ed (make-text-input "hello")) (data (component-data ed)))
  (check (%ed-sync-viewport! data 40) => 0)
) ;let*

(check-report)
