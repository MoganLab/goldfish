;; bigcomplex?
;; 判断对象是否为 <bigcomplex> 复数。
;;
;; 语法
;; ----
;; (bigcomplex? x)
;;
;; 参数
;; ----
;; x : any? 任意对象。
;;
;; 返回值
;; ----
;; boolean? 是 <bigcomplex> 返回 #t，否则返回 #f。

(import (liii check) (liii bigcomplex))

(check-set-mode! 'report-failed)

(check (bigcomplex? (make-bigcomplex 1 2)) => #t)
(check (bigcomplex? 42) => #f)

(check-report)
