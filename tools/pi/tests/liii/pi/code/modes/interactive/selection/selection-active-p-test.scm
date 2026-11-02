;; selection-active?
;; 判断选区范围当前是否处于激活选中文本状态。
;;
;; 语法
;; ----
;; (selection-active? sel)
;;
;; 参数
;; ----
;; sel : selection-range?
;; 选区范围记录对象。
;;
;; 返回值
;; -----
;; boolean?
;; 激活返回 #t，未激活返回 #f。

(import (scheme base) (liii check) (liii pi code modes interactive selection))

(check-set-mode! 'report-failed)

(let ((sel-active (make-selection-range '(0 . 0) '(5 . 5) #t))
      (sel-inactive (make-selection-range '(0 . 0) '(5 . 5) #f))
     ) ;
  (check-true (selection-active? sel-active))
  (check-false (selection-active? sel-inactive))
) ;let

(check-report)
