;; pi-chain-node-key
;; 生成节点的唯一键字符串，格式为 provider/model:thinkingLevel。
;;
;; 语法
;; ----
;; (pi-chain-node-key node)
;;
;; 参数
;; ----
;; node : <chain-node> | string | any
;; 链节点记录或标识。
;;
;; 返回值
;; ----
;; string
;; 节点的唯一标识字符串。

(import (scheme base)
  (liii check)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(let ((node1 (make-chain-node "kimi" "k3" "high"))
      (node2 (make-chain-node "deepseek" "chat"))
     ) ;
  (check (pi-chain-node-key node1) => "kimi/k3:high")
  (check (pi-chain-node-key node2) => "deepseek/chat:off")
  (check (pi-chain-node-key "custom/node:low") => "custom/node:low")
) ;let

(check-report)
