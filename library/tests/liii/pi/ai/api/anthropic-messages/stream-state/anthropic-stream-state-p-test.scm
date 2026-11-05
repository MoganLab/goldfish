;; anthropic-stream-state?
;; 判断对象是否为 Anthropic 流状态记录。
;;
;; 语法
;; ----
;; (anthropic-stream-state? state)
;;
;; 参数
;; ----
;; state : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 是流状态记录返回 #t，否则返回 #f。

(import (liii check)
  (liii pi ai api anthropic-messages stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 流状态记录
(check (anthropic-stream-state?
         (make-anthropic-stream-state '() #f '() '() "end_turn" #f #f))
  => #t)

;; 其它对象
(check (anthropic-stream-state? '()) => #f)
(check (anthropic-stream-state? #f) => #f)
(check (anthropic-stream-state? "state") => #f)
(check (anthropic-stream-state? 42) => #f)

(check-report)
