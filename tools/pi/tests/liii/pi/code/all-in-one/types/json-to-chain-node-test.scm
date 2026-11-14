;; json->chain-node
;; 将 JSON 对象转换为 <chain-node> record。
;;
;; 语法
;; ----
;; (json->chain-node obj)
;; (json->chain-node obj chain-id)
;;
;; 参数
;; ----
;; obj : any?
;; string->json 解析出的 alist，或已是 <chain-node> 的 record。
;; chain-id : string? (可选)
;; 用于告警日志的所属链 ID。
;;
;; 返回值
;; ----
;; chain-node? | #f
;; 解析成功返回 <chain-node>，非对象或缺少必填字段 provider/model 时返回 #f。

(import (liii check) (liii json) (liii pi code all-in-one types))

(check-set-mode! 'report-failed)

(check-true (procedure? json->chain-node))

;; 1. 完整节点解析
(let* ((j (string->json "{\"provider\":\"deepseek\",\"model\":\"deepseek-chat\",\"thinkingLevel\":\"high\"}"))
       (n (json->chain-node j "fast"))
      ) ;
  (check-true (chain-node? n))
  (check (chain-node-provider n) => "deepseek")
  (check (chain-node-model n) => "deepseek-chat")
  (check (chain-node-thinking-level n) => "high")
) ;let*

;; 2. 缺省或空白 thinkingLevel 回退为 "off"
(let ((n1 (json->chain-node (string->json "{\"provider\":\"google\",\"model\":\"gemini-2.5-flash\"}")))
      (n2 (json->chain-node (string->json "{\"provider\":\"google\",\"model\":\"gemini-2.5-flash\",\"thinkingLevel\":\"  \"}")))
     ) ;
  (check (chain-node-thinking-level n1) => "off")
  (check (chain-node-thinking-level n2) => "off")
) ;let

;; 3. 非法输入返回 #f
(check-false (json->chain-node '()))
(check-false (json->chain-node "bad" "fast"))
(check-false (json->chain-node (string->json "{\"model\":\"m\"}") "fast"))
(check-false (json->chain-node (string->json "{\"provider\":\"p\",\"model\":\"  \"}") "fast"))

;; 4. 已是 record 原样返回
(let* ((orig (make-chain-node "openai" "gpt-5" "low"))
       (out (json->chain-node orig))
      ) ;
  (check (eq? out orig) => #t)
) ;let*

(check-report)
