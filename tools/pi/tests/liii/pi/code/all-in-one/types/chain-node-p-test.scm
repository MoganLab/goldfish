;; chain-node?
;; 判定对象是否为链节点记录。
;;
;; 语法
;; ----
;; (chain-node? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 为链节点时返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi code all-in-one types))

(check-set-mode! 'report-failed)

(check-true (chain-node? (make-chain-node "p" "m")))
(check-false (chain-node? "not-a-node"))
(check-false (chain-node? 123))

(check-report)
