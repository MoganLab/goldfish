;; selection-focus
;; 获取选区范围对象的当前焦点坐标。
;;
;; 语法
;; ----
;; (selection-focus sel)
;;
;; 参数
;; ----
;; sel : selection-range?
;; 选区范围记录对象。
;;
;; 返回值
;; -----
;; pair?
;; 焦点坐标点对 (x . y)。

(import (scheme base) (liii check) (liii pi code modes interactive selection))

(check-set-mode! 'report-failed)

(let ((sel (make-selection-range '(3 . 7) '(12 . 9) #t)))
  (check (selection-focus sel) => '(12 . 9))
) ;let

(check-report)
