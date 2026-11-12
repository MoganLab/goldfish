;; pi-chain-node-last-failure
;; 查询节点的最近一次故障时间戳（单调毫秒），无故障记录时返回 #f。
;;
;; 语法
;; ----
;; (pi-chain-node-last-failure node)
;;
;; 参数
;; ----
;; node : <chain-node> | string
;; 链节点记录或标识。
;;
;; 返回值
;; ----
;; number | #f
;; 节点最近一次故障的时间戳（毫秒）；未曾故障或已清除时返回 #f。

(import (scheme base)
  (liii check)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(pi-chain-reset-cooldown-state!)

(let ((node (make-chain-node "kimi" "k3" "high")))
  (check-false (pi-chain-node-last-failure node))
  (pi-chain-record-node-failure! node 12345)
  (check (pi-chain-node-last-failure node) => 12345)
  (pi-chain-clear-node-failure! node)
  (check-false (pi-chain-node-last-failure node))
) ;let

(pi-chain-reset-cooldown-state!)

(check-report)
