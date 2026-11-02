;; thinking-selector?
;; 判定对象是否为思维链等级选择器组件实例。
;;
;; 语法
;; ----
;; (thinking-selector? obj)
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
  (liii tui)
  (liii pi code modes interactive thinking-selector)
) ;import

(check-set-mode! 'report-failed)

;; 1. make-thinking-selector 返回的组件为 #t
(let ((ts (make-thinking-selector "off"
            '("off" "low" "high")
            "off"
            (lambda (l) #t)
            (lambda () #t)
            (lambda (l) #t)
          ) ;make-thinking-selector
      ) ;ts
     ) ;
  (check-true (thinking-selector? ts))
) ;let

;; 2. 非选择器组件为 #f
(check-false
  (thinking-selector? (make-component :render (lambda (s w) '())))
) ;check-false
(check-false (thinking-selector? #f))
(check-false (thinking-selector? "selector"))

(check-report)
