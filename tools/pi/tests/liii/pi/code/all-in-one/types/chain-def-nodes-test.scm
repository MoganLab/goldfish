;; chain-def-nodes
;; 获取链定义的节点列表。
;;
;; 语法
;; ----
;; (chain-def-nodes chain)
;;
;; 参数
;; ----
;; chain : <chain-def>
;; 链定义记录。
;;
;; 返回值
;; ----
;; list of <chain-node>
;; 节点记录列表。

(import (scheme base) (liii check) (liii pi code all-in-one types))

(check-set-mode! 'report-failed)

(let* ((nodes (list (make-chain-node "p" "m")))
       (c (make-chain-def "c1" "链1" nodes))
      ) ;
  (check (length (chain-def-nodes c)) => 1)
  (check (chain-node-provider (car (chain-def-nodes c))) => "p")
) ;let*

(check-report)
