;; status-card?
;; 判定对象是否为状态提示卡片组件实例。
;;
;; 语法
;; ----
;; (status-card? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检验的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 满足条件返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii pi code modes interactive status-card)
) ;import

(check-set-mode! 'report-failed)

;; 1. make-status-card 构造的组件判定为 #t
(let ((card (make-status-card "Ready")))
  (check-true (status-card? card))
) ;let

;; 2. 非状态卡片或非组件对象判定为 #f
(check-false
  (status-card? (make-component :render (lambda (self w) '())))
) ;check-false
(check-false (status-card? "not a card"))
(check-false (status-card? #f))
(check-false (status-card? '()))

(check-report)
