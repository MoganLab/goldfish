;; json->chain-def
;; 将 JSON 对象转换为 <chain-def> record。
;;
;; 语法
;; ----
;; (json->chain-def obj)
;;
;; 参数
;; ----
;; obj : any?
;; string->json 解析出的 alist，或已是 <chain-def> 的 record。
;;
;; 返回值
;; ----
;; chain-def? | #f
;; 解析成功返回 <chain-def>，非对象或缺少必填字段 id 时返回 #f；
;; 内部无效的 node 会被过滤掉。

(import (liii check) (liii json) (liii pi code all-in-one types))

(check-set-mode! 'report-failed)

(check-true (procedure? json->chain-def))

;; 1. 完整链定义解析（含无效节点过滤）
(let* ((j (string->json "{\"id\":\"fast\",\"name\":\"Fast Chain\",\"nodes\":[{\"provider\":\"deepseek\",\"model\":\"deepseek-chat\"},{\"provider\":\"bad\"},{\"provider\":\"google\",\"model\":\"gemini-2.5-flash\",\"thinkingLevel\":\"low\"}]}"))
       (c (json->chain-def j))
      ) ;
  (check-true (chain-def? c))
  (check (chain-def-id c) => "fast")
  (check (chain-def-name c) => "Fast Chain")
  (check (length (chain-def-nodes c)) => 2)
  (check (chain-node-provider (car (chain-def-nodes c))) => "deepseek")
  (check (chain-node-thinking-level (cadr (chain-def-nodes c))) => "low")
) ;let*

;; 2. 缺省或空白 name 回退为 id；nodes 非数组时为空列表
(let ((c (json->chain-def (string->json "{\"id\":\"smart\",\"name\":\"  \",\"nodes\":\"bad\"}"))))
  (check-true (chain-def? c))
  (check (chain-def-name c) => "smart")
  (check (chain-def-nodes c) => '())
) ;let

;; 3. 非法输入返回 #f
(check-false (json->chain-def '()))
(check-false (json->chain-def "bad"))
(check-false (json->chain-def (string->json "{\"name\":\"no-id\"}")))
(check-false (json->chain-def (string->json "{\"id\":\"  \"}")))

;; 4. 已是 record 原样返回
(let* ((orig (make-chain-def "c1" "Chain 1" '()))
       (out (json->chain-def orig))
      ) ;
  (check (eq? out orig) => #t)
) ;let*

(check-report)
