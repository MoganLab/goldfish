;; tool-card-component?
;; 判定对象是否为工具调用卡片组件实例。
;;
;; 语法
;; ----
;; (tool-card-component? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检验的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 若为工具卡片组件返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

;; 1. make-tool-card 返回组件判定为 #t
(let ((card (make-tool-card "bash" '((command . "ls")) 'running)))
  (check-true (tool-card-component? card))
) ;let

;; 2. 非工具卡片组件判定为 #f
(check-false
  (tool-card-component? (make-component :render (lambda (s w) '())))
) ;check-false
(check-false (tool-card-component? #f))
(check-false (tool-card-component? "tool"))

(check-report)
