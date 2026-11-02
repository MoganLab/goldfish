;; thinking-card-component?
;; 判定对象是否为思维链卡片 TUI 组件实例。
;;
;; 语法
;; ----
;; (thinking-card-component? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检验的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 若为 thinking 卡片组件返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii pi code modes interactive views thinking)
) ;import

(check-set-mode! 'report-failed)

;; 1. make-thinking-card 返回组件判定为 #t
(let ((card (make-thinking-card "step 1")))
  (check-true (thinking-card-component? card))
) ;let

;; 2. 非 thinking-card 组件判定为 #f
(check-false
  (thinking-card-component? (make-component :render (lambda (s w) '())))
) ;check-false
(check-false (thinking-card-component? #f))
(check-false (thinking-card-component? "card"))

(check-report)
