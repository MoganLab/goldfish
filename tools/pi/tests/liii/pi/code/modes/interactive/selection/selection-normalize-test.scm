;; selection-normalize
;; 将任意方向（反向、跨行）的选区端点规整为由上至下、由左至右的标准顺序 (values start end)。
;;
;; 语法
;; ----
;; (selection-normalize sel)
;; (selection-normalize p1 p2)
;;
;; 参数
;; ----
;; sel : selection-range?
;; 选区范围记录对象。
;; p1 : pair?
;; 第一点坐标 (x . y)。
;; p2 : pair?
;; 第二点坐标 (x . y)。
;;
;; 返回值
;; -----
;; (values start end)
;; 规范顺序的起始坐标与结束坐标点对。

(import (scheme base) (liii check) (liii pi code modes interactive selection))

(check-set-mode! 'report-failed)

;; 1. 同行正向与反向
(let* ((sel1 (make-selection-range '(2 . 5) '(8 . 5) #t))
       (sel2 (make-selection-range '(8 . 5) '(2 . 5) #t))
      ) ;
  (let-values (((s1 e1) (selection-normalize sel1)) ((s2 e2) (selection-normalize sel2)))
    (check s1 => '(2 . 5))
    (check e1 => '(8 . 5))
    (check s2 => '(2 . 5))
    (check e2 => '(8 . 5))
  ) ;let-values
) ;let*

;; 2. 跨行反向（自下而上）
(let* ((sel (make-selection-range '(10 . 8) '(2 . 3) #t)))
  (let-values (((start end) (selection-normalize sel)))
    (check start => '(2 . 3))
    (check end => '(10 . 8))
  ) ;let-values
) ;let*

;; 3. 直接传入两点重载
(let-values (((start end) (selection-normalize '(12 . 8) '(3 . 1))))
  (check start => '(3 . 1))
  (check end => '(12 . 8))
) ;let-values

(check-report)
