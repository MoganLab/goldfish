;; chain-node->json
;; 将 <chain-node> record 转换为 Scheme-JSON 对象（alist）。
;;
;; 语法
;; ----
;; (chain-node->json node) -> alist
;;
;; 参数
;; ----
;; node : chain-node? | alist
;; 链节点 record 或已是 JSON alist。
;;
;; 返回值
;; ----
;; alist
;; 可由 json->string 序列化的 JSON 对象，且可被 json->chain-node 往返还原。

(import (scheme base)
  (liii check)
  (liii json)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? chain-node->json))

(let* ((orig (make-chain-node "deepseek" "deepseek-chat" "high"))
       (j (chain-node->json orig))
       (rt (json->chain-node (string->json (json->string j))))
      ) ;
  (check (cdr (assoc "provider" j)) => "deepseek")
  (check (cdr (assoc "model" j)) => "deepseek-chat")
  (check (cdr (assoc "thinkingLevel" j)) => "high")
  (check-true (chain-node? rt))
  (check (chain-node-provider rt) => "deepseek")
  (check (chain-node-model rt) => "deepseek-chat")
  (check (chain-node-thinking-level rt) => "high")
) ;let*

(check-catch 'type-error (chain-node->json '(("provider" . "x"))))
(check-catch 'type-error (chain-node->json "invalid"))

(check-report)
