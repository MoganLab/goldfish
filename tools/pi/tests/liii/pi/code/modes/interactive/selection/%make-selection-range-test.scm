;; %make-selection-range
;; 底层构造鼠标选区记录实例。
;;
;; 语法
;; ----
;; (%make-selection-range anchor focus active?)
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

(let ((sel (%make-selection-range '(0 . 0) '(5 . 5) #f)))
  (check-true (selection-range? sel))
  (check (selection-anchor sel) => '(0 . 0))
  (check (selection-focus sel) => '(5 . 5))
  (check-false (selection-active? sel))
) ;let

(check-report)
