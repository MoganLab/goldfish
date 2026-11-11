;; make-chain-node
;; 构造链节点记录。
;;
;; 语法
;; ----
;; (make-chain-node provider model [thinking-level])
;;
;; 参数
;; ----
;; provider : string
;; 供应商标识。
;; model : string
;; 模型名称。
;; thinking-level : string (可选，缺省为 "off")
;; 思考等级。
;;
;; 返回值
;; ----
;; <chain-node>
;; 链节点记录对象。

(import (scheme base) (liii check) (liii pi all_in_one types))

(check-set-mode! 'report-failed)

(let ((n1 (make-chain-node "kimi" "k3")) (n2 (make-chain-node "ds" "chat" "high")))
  (check-true (chain-node? n1))
  (check (chain-node-thinking-level n1) => "off")
  (check (chain-node-thinking-level n2) => "high")
) ;let

(check-report)
