;; make-selection-range
;; 构造鼠标选区范围对象（包含锚点、焦点与激活状态）。
;;
;; 语法
;; ----
;; (make-selection-range anchor focus active?)
;;
;; 参数
;; ----
;; anchor : pair?
;; 选区起始锚点坐标 (x . y)。
;; focus : pair?
;; 选区当前焦点坐标 (x . y)。
;; active? : boolean?
;; 选区当前是否已激活。
;;
;; 返回值
;; -----
;; selection-range?
;; 选区范围记录对象。

(import (scheme base) (liii check) (liii pi code modes interactive selection))

(check-set-mode! 'report-failed)

(let ((sel (make-selection-range '(2 . 3) '(10 . 5) #t)))
  (check-true (selection-range? sel))
  (check (selection-anchor sel) => '(2 . 3))
  (check (selection-focus sel) => '(10 . 5))
  (check-true (selection-active? sel))
) ;let

(check-report)
