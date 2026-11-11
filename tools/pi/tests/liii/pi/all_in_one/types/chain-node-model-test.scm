;; chain-node-model
;; 获取链节点的模型名称。
;;
;; 语法
;; ----
;; (chain-node-model node)
;;
;; 参数
;; ----
;; node : <chain-node>
;; 链节点记录。
;;
;; 返回值
;; ----
;; string
;; 模型名称。

(import (scheme base) (liii check) (liii pi all_in_one types))

(check-set-mode! 'report-failed)

(let ((n (make-chain-node "kimi1" "k3")))
  (check (chain-node-model n) => "k3")
) ;let

(check-report)
