;; pi-chain-node-in-cooldown?
;; 判定节点当前是否处于故障冷却期内。
;;
;; 语法
;; ----
;; (pi-chain-node-in-cooldown? node cooldown-ms [now-ms])
;;
;; 参数
;; ----
;; node : <chain-node> | string
;; 要检查的链节点。
;; cooldown-ms : number
;; 冷却时间窗口毫秒数；<= 0 时始终返回 #f。
;; now-ms : number (可选)
;; 当前时间戳（毫秒）；未提供时使用系统当前单调毫秒。
;;
;; 返回值
;; ----
;; boolean
;; 若节点在冷却窗口内返回 #t，未曾故障、已超时或冷却已禁用时返回 #f。

(import (scheme base)
  (liii check)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(pi-chain-reset-cooldown-state!)

(let ((node (make-chain-node "kimi" "k3" "high")))
  ;; 从未故障的节点不在冷却期
  (check-false (pi-chain-node-in-cooldown? node 60000 10000))

  ;; 在 T=1000 记录故障
  (pi-chain-record-node-failure! node 1000)

  ;; T=5000: 距离故障 4 秒 < 60 秒冷却期，处于冷却中
  (check-true (pi-chain-node-in-cooldown? node 60000 5000))

  ;; T=60999: 距离故障 59.999 秒 < 60 秒冷却期，仍处于冷却中
  (check-true (pi-chain-node-in-cooldown? node 60000 60999))

  ;; T=61000: 距离故障正好 60 秒，冷却期结束
  (check-false (pi-chain-node-in-cooldown? node 60000 61000))

  ;; T=70000: 距离故障 69 秒，已超出冷却期
  (check-false (pi-chain-node-in-cooldown? node 60000 70000))

  ;; cooldown-ms <= 0 时冷却禁用，返回 #f
  (check-false (pi-chain-node-in-cooldown? node 0 5000))
  (check-false (pi-chain-node-in-cooldown? node -1 5000))

  ;; 清除故障后不再处于冷却期
  (pi-chain-clear-node-failure! node)
  (check-false (pi-chain-node-in-cooldown? node 60000 5000))
) ;let

(pi-chain-reset-cooldown-state!)

(check-report)
