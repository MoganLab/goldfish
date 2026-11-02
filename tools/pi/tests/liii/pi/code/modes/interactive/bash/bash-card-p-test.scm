;; bash-card?
;; 判定对象是否为 Bash 命令卡片底层 record 数据结构。
;;
;; 语法
;; ----
;; (bash-card? obj)
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
  (liii pi code modes interactive bash)
) ;import

(check-set-mode! 'report-failed)

;; 1. 组件内部的 component-data 为 bash-card record
(let ((card (make-bash-card "ls -la")))
  (check-true (bash-card? (component-data card)))
) ;let

;; 2. 非 record 对象判定为 #f
(check-false (bash-card? #f))
(check-false (bash-card? 123))

(check-report)
