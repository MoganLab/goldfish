;; pi-chain-record-node-failure!
;; 记录或刷新节点的故障时间戳。
;;
;; 语法
;; ----
;; (pi-chain-record-node-failure! node [timestamp-ms])
;;
;; 参数
;; ----
;; node : <chain-node> | string
;; 发生故障的链节点。
;; timestamp-ms : number (可选)
;; 指定的故障时间戳（毫秒）；未提供时使用系统当前单调毫秒。
;;
;; 返回值
;; ----
;; unspecified
;; 更新节点的故障时间戳。

(import (scheme base)
  (liii check)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(pi-chain-reset-cooldown-state!)

(let ((node (make-chain-node "deepseek" "chat" "medium")))
  (pi-chain-record-node-failure! node 20000)
  (check (pi-chain-node-last-failure node) => 20000)
  ;; 刷新故障时间戳
  (pi-chain-record-node-failure! node 30000)
  (check (pi-chain-node-last-failure node) => 30000)
) ;let

;; 缺省时间戳取当前系统时间
(let ((node2 (make-chain-node "google" "gemini" "off")))
  (pi-chain-record-node-failure! node2)
  (check-true (number? (pi-chain-node-last-failure node2)))
  (check-true (> (pi-chain-node-last-failure node2) 0))
) ;let

(pi-chain-reset-cooldown-state!)

(check-report)
