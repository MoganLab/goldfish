(import (liii check) (scheme lazy))
(check-set-mode! 'report-failed)

;; promise?
;; 判断对象是否为 promise。
;;
;; 语法
;; ----
;; (promise? obj)
;;
;; 返回值
;; ----
;; boolean?

(check (promise? (delay 1)) => #t)
(check (promise? (make-promise 'x)) => #t)
(check (promise? 5) => #f)
(check (promise? '()) => #f)
(check (promise? (list 1 2)) => #f)

(check-report)
