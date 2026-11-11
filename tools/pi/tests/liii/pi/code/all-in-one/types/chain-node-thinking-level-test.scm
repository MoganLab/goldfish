;; chain-node-thinking-level
;; 获取链节点的思考等级。
;;
;; 语法
;; ----
;; (chain-node-thinking-level node)
;;
;; 参数
;; ----
;; node : <chain-node>
;; 链节点记录。
;;
;; 返回值
;; ----
;; string
;; 思考等级。

(import (scheme base) (liii check) (liii pi code all-in-one types))

(check-set-mode! 'report-failed)

(let ((n1 (make-chain-node "p" "m")) (n2 (make-chain-node "p" "m" "medium")))
  (check (chain-node-thinking-level n1) => "off")
  (check (chain-node-thinking-level n2) => "medium")
) ;let

(check-report)
