;; chain-node-provider
;; 获取链节点的供应商标识。
;;
;; 语法
;; ----
;; (chain-node-provider node)
;;
;; 参数
;; ----
;; node : <chain-node>
;; 链节点记录。
;;
;; 返回值
;; ----
;; string
;; 供应商标识。

(import (scheme base) (liii check) (liii pi code all-in-one types))

(check-set-mode! 'report-failed)

(let ((n (make-chain-node "kimi1" "k3")))
  (check (chain-node-provider n) => "kimi1")
) ;let

(check-report)
