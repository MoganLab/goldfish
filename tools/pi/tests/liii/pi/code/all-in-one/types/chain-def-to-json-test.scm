;; chain-def->json
;; 将 <chain-def> record 转换为 Scheme-JSON 对象（alist）。
;;
;; 语法
;; ----
;; (chain-def->json chain) -> alist
;;
;; 参数
;; ----
;; chain : chain-def? | alist
;; 链定义 record 或已是 JSON alist。
;;
;; 返回值
;; ----
;; alist
;; 可由 json->string 序列化的 JSON 对象，且可被 json->chain-def 往返还原。

(import (scheme base)
  (liii check)
  (liii json)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? chain-def->json))

(let* ((orig (make-chain-def "fast"
               "Fast Chain"
               (list (make-chain-node "google" "gemini-3.8-flash" "medium")
                 (make-chain-node "deepseek" "deepseek-chat" "off")
               ) ;list
             ) ;make-chain-def
       ) ;orig
       (j (chain-def->json orig))
       (rt (json->chain-def (string->json (json->string j))))
      ) ;
  (check (cdr (assoc "id" j)) => "fast")
  (check (cdr (assoc "name" j)) => "Fast Chain")
  (check-true (vector? (cdr (assoc "nodes" j))))
  (check (vector-length (cdr (assoc "nodes" j))) => 2)
  (check-true (chain-def? rt))
  (check (chain-def-id rt) => "fast")
  (check (chain-def-name rt) => "Fast Chain")
  (check (length (chain-def-nodes rt)) => 2)
  (check (chain-node-thinking-level (car (chain-def-nodes rt))) => "medium")
) ;let*

(check-catch 'type-error (chain-def->json '(("id" . "x"))))
(check-catch 'type-error (chain-def->json "invalid"))

(check-report)
