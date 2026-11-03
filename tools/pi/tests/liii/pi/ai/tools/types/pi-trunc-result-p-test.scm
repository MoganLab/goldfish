;; pi-trunc-result?
;; 判断对象是否为文本截断结果 record 实例。
;;
;; 语法
;; ----
;; (pi-trunc-result? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于文本截断结果 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai tools types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-trunc-result "line1\nline2" #t "max-lines" 100 2048 2 16 #t #f))
     ) ;
  (check-true (pi-trunc-result? obj))
) ;let
(check-false (pi-trunc-result? '()))
(check-false (pi-trunc-result? #f))
(check-false (pi-trunc-result? "not-a-record"))

(check-report)
