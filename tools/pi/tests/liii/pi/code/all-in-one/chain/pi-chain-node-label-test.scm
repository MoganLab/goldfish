;; pi-chain-node-label
;; 返回用于切换与回溯日志的 provider/model 标签。
;;
;; 语法
;; ----
;; (pi-chain-node-label node)
;;
;; 参数
;; ----
;; node : <chain-node>
;; 链节点。
;;
;; 返回值
;; ----
;; string
;; provider/model 形式标签。

(import (scheme base) (liii check) (liii pi code all-in-one chain)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)
(check (pi-chain-node-label (make-chain-node "kimi1" "k3")) => "kimi1/k3")

(check-report)
