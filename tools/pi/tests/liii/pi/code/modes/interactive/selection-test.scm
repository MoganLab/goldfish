(import (scheme base)
        (liii check)
        (liii pi code modes interactive selection))

(check-set-mode! 'report-failed)

;; 1. 结构体与字段访问
(let ((sel (make-selection-range '(2 . 3) '(10 . 5) #t)))
  (check-true (selection-range? sel))
  (check (selection-anchor sel) => '(2 . 3))
  (check (selection-focus sel) => '(10 . 5))
  (check-true (selection-active? sel)))

(let ((sel (make-selection-range '(0 . 0) '(0 . 0) #f)))
  (check-false (selection-active? sel)))

;; 2. 同行正向拖拽规整
(let* ((sel (make-selection-range '(2 . 5) '(8 . 5) #t)))
  (let-values (((start end) (selection-normalize sel)))
    (check start => '(2 . 5))
    (check end => '(8 . 5)))
  (check (selection-start sel) => '(2 . 5))
  (check (selection-end sel) => '(8 . 5)))

;; 3. 同行反向拖拽规整（从右向左）
(let* ((sel (make-selection-range '(8 . 5) '(2 . 5) #t)))
  (let-values (((start end) (selection-normalize sel)))
    (check start => '(2 . 5))
    (check end => '(8 . 5)))
  (check (selection-start sel) => '(2 . 5))
  (check (selection-end sel) => '(8 . 5)))

;; 4. 跨行正向拖拽（自上而下、向左下）
(let* ((sel (make-selection-range '(10 . 2) '(5 . 6) #t)))
  (let-values (((start end) (selection-normalize sel)))
    (check start => '(10 . 2))
    (check end => '(5 . 6)))
  (check (selection-start sel) => '(10 . 2))
  (check (selection-end sel) => '(5 . 6)))

;; 5. 跨行反向拖拽（自下而上、向右上）
(let* ((sel (make-selection-range '(5 . 6) '(10 . 2) #t)))
  (let-values (((start end) (selection-normalize sel)))
    (check start => '(10 . 2))
    (check end => '(5 . 6)))
  (check (selection-start sel) => '(10 . 2))
  (check (selection-end sel) => '(5 . 6)))

;; 6. 原地（锚点与焦点重合）
(let* ((sel (make-selection-range '(4 . 4) '(4 . 4) #t)))
  (let-values (((start end) (selection-normalize sel)))
    (check start => '(4 . 4))
    (check end => '(4 . 4))))

;; 7. 直接传入两坐标的重载形式
(let-values (((start end) (selection-normalize '(12 . 8) '(3 . 1))))
  (check start => '(3 . 1))
  (check end => '(12 . 8)))

(check-report)
