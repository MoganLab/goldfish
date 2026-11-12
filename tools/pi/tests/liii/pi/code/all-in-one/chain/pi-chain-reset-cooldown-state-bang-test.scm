;; pi-chain-reset-cooldown-state!
;; 清空所有节点的故障时间戳，解除所有节点的冷却状态。
;;
;; 语法
;; ----
;; (pi-chain-reset-cooldown-state!)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; unspecified
;; 清空故障记录映射。

(import (scheme base)
  (liii check)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(let ((node1 (make-chain-node "p1" "m1"))
      (node2 (make-chain-node "p2" "m2"))
     ) ;
  (pi-chain-record-node-failure! node1 1000)
  (pi-chain-record-node-failure! node2 2000)
  (check-true (pi-chain-node-in-cooldown? node1 60000 5000))
  (check-true (pi-chain-node-in-cooldown? node2 60000 5000))

  (pi-chain-reset-cooldown-state!)

  (check-false (pi-chain-node-in-cooldown? node1 60000 5000))
  (check-false (pi-chain-node-in-cooldown? node2 60000 5000))
  (check-false (pi-chain-node-last-failure node1))
  (check-false (pi-chain-node-last-failure node2))
) ;let

(check-report)
