;; make-chain-def
;; 构造链定义记录。
;;
;; 语法
;; ----
;; (make-chain-def id name nodes)
;;
;; 参数
;; ----
;; id : string
;; 链唯一标识。
;; name : string
;; 链展示名称。
;; nodes : list
;; 节点列表。
;;
;; 返回值
;; ----
;; <chain-def>
;; 链定义记录对象。

(import (scheme base) (liii check) (liii pi all_in_one types))

(check-set-mode! 'report-failed)

(let ((c (make-chain-def "c1" "链1" '())))
  (check-true (chain-def? c))
  (check (chain-def-id c) => "c1")
  (check (chain-def-name c) => "链1")
  (check (chain-def-nodes c) => '())
) ;let

(check-report)
