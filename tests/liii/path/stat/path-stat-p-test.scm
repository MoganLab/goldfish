(import (liii check)
  (liii os)
  (liii path stat)
) ;import

(check-set-mode! 'report-failed)

;; path-stat?
;; 判断对象是否为 <path-stat> 记录对象。
;;
;; 语法
;; ----
;; (path-stat? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; -----
;; boolean
;; 当 obj 是 <path-stat> 记录时返回 #t，否则返回 #f。

(let ((st (make-path-stat 10 1.0 2.0 3.0 0)))
  (check-true (path-stat? st))
) ;let

(let ((real-st (path-stat (os-temp-dir))))
  (check-true (path-stat? real-st))
) ;let

(check-false (path-stat? 123))
(check-false (path-stat? "string"))
(check-false (path-stat? '(1 2 3)))
(check-false (path-stat? #(1 2 3 4 5)))
(check-false (path-stat? #t))
(check-false (path-stat? #f))

(check-report)
