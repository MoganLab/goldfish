;; %ed-viewport
;; 计算编辑器当前的视口参数，包括起始行、可视行数及上下隐藏行数。
;;
;; 语法
;; ----
;; (%ed-viewport data width)
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
;; list?
;; 包含 (top-row visible-count above-hidden below-hidden) 的四元列表。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui editor component)
  (liii tui editor viewport)
) ;import

(check-set-mode! 'report-failed)

(let* ((ed (make-text-input "hello"))
       (data (component-data ed))
       (vp (%ed-viewport data 40))
      ) ;
  (check (list? vp) => #t)
  (check (length vp) => 4)
  (check (car vp) => 0)
) ;let*

(check-report)
