;; thinking-card?
;; 判定对象是否为思维链卡片底层 record 数据结构。
;;
;; 语法
;; ----
;; (thinking-card? obj)
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
  (liii pi code modes interactive views thinking)
) ;import

(check-set-mode! 'report-failed)

;; 1. 组件内部的 component-data 为 thinking-card record
(let ((card (make-thinking-card "thinking step 1")))
  (check-true (thinking-card? (component-data card)))
) ;let

;; 2. 非 record 对象返回 #f
(check-false (thinking-card? #f))
(check-false (thinking-card? 123))

(check-report)
