;; selection-anchor
;; 获取选区范围对象的起始锚点坐标。
;;
;; 语法
;; ----
;; (selection-anchor sel)
;;
;; 参数
;; ----
;; sel : selection-range?
;; 选区范围记录对象。
;;
;; 返回值
;; -----
;; pair?
;; 锚点坐标点对 (x . y)。

(import (scheme base) (liii check) (liii pi code modes interactive selection))

(check-set-mode! 'report-failed)

(let ((sel (make-selection-range '(3 . 7) '(12 . 9) #t)))
  (check (selection-anchor sel) => '(3 . 7))
) ;let

(check-report)
