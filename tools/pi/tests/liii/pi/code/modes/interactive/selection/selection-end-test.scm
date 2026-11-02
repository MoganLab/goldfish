;; selection-end
;; 获取规整后选区的结束端点坐标 (x . y)。
;;
;; 语法
;; ----
;; (selection-end sel)
;;
;; 参数
;; ----
;; sel : selection-range?
;; 选区范围记录对象。
;;
;; 返回值
;; -----
;; pair?
;; 结束端点坐标点对 (x . y)。

(import (scheme base) (liii check) (liii pi code modes interactive selection))

(check-set-mode! 'report-failed)

;; 无论拖拽方向为何，end 恒为规范顺序末端点
(let ((sel1 (make-selection-range '(2 . 5) '(8 . 5) #t))
      (sel2 (make-selection-range '(8 . 5) '(2 . 5) #t))
      (sel3 (make-selection-range '(10 . 8) '(2 . 3) #t))
     ) ;
  (check (selection-end sel1) => '(8 . 5))
  (check (selection-end sel2) => '(8 . 5))
  (check (selection-end sel3) => '(10 . 8))
) ;let

(check-report)
