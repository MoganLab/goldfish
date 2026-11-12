;; pi-chain-clear-node-failure!
;; 清除指定节点的故障记录。
;;
;; 语法
;; ----
;; (pi-chain-clear-node-failure! node)
;;
;; 参数
;; ----
;; node : <chain-node> | string
;; 要清除故障记录的链节点。
;;
;; 返回值
;; ----
;; unspecified
;; 清除该节点的故障记录。

(import (scheme base)
  (liii check)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(pi-chain-reset-cooldown-state!)

(let ((node1 (make-chain-node "p1" "m1"))
      (node2 (make-chain-node "p2" "m2"))
     ) ;
  (pi-chain-record-node-failure! node1 1000)
  (pi-chain-record-node-failure! node2 2000)
  (check (pi-chain-node-last-failure node1) => 1000)
  (check (pi-chain-node-last-failure node2) => 2000)

  (pi-chain-clear-node-failure! node1)
  (check-false (pi-chain-node-last-failure node1))
  ;; node2 不受影响
  (check (pi-chain-node-last-failure node2) => 2000)
) ;let

(pi-chain-reset-cooldown-state!)

(check-report)
