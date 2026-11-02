;; pi-header?
;; 判定对象是否为页眉（Header）组件实例。
;;
;; 语法
;; ----
;; (pi-header? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判定的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 若为交互模式页眉组件则返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii pi code modes interactive header)
) ;import

(check-set-mode! 'report-failed)

;; 1. make-pi-header 构造的实例为真
(check-true (pi-header? (make-pi-header)))
(check-true (pi-header? (make-pi-header :version "1.0.0")))

;; 2. 非页眉组件或非组件对象为假
(check-false
  (pi-header? (make-component :render (lambda (self w) '())))
) ;check-false
(check-false (pi-header? #f))
(check-false (pi-header? 123))
(check-false (pi-header? "header"))
(check-false (pi-header? '()))

(check-report)
