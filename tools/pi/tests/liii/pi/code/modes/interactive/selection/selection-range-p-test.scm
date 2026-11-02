;; selection-range?
;; 判断对象是否为选区范围记录实例。
;;
;; 语法
;; ----
;; (selection-range? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检验的对象。
;;
;; 返回值
;; -----
;; boolean?
;; 若为选区记录则返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi code modes interactive selection))

(check-set-mode! 'report-failed)

(let ((sel (make-selection-range '(0 . 0) '(1 . 1) #t)))
  (check-true (selection-range? sel))
) ;let

(check-false (selection-range? '(0 . 0)))
(check-false (selection-range? "range"))
(check-false (selection-range? #f))

(check-report)
