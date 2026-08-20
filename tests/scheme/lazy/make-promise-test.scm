(import (liii check) (scheme lazy))
(check-set-mode! 'report-failed)

;; make-promise
;; 将对象包装为 promise。
;;
;; 语法
;; ----
;; (make-promise obj)
;;
;; 返回值
;; ----
;; promise
;; 若 obj 已是 promise 则原样返回。

(check (promise? (make-promise 5)) => #t)
(check (force (make-promise 5)) => 5)

(define p (delay 1))
(check (eq? (make-promise p) p) => #t)
(check (eq? (make-promise 5) 5) => #f)

(check-report)
