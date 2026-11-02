;; tool-card?
;; 判定对象是否为工具卡片底层的 record 数据结构。
;;
;; 语法
;; ----
;; (tool-card? obj)
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
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

;; 1. 组件内部的 component-data 为 tool-card record
(let ((card (make-tool-card "bash" '((command . "ls")) 'running)))
  (check-true (tool-card? (component-data card)))
) ;let

;; 2. 非 record 对象判定为 #f
(check-false (tool-card? #f))
(check-false (tool-card? 123))

(check-report)
