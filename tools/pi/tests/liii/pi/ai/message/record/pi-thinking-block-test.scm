;; pi-thinking-block
;; 构造 thinking 类型的 content block record 对象。
;;
;; 语法
;; ----
;; (pi-thinking-block thinking)
;; (pi-thinking-block thinking signature)
;;
;; 参数
;; ----
;; thinking : string?
;; 思考过程正文。
;;
;; signature : string? 或 #f
;; 可选，思考签名。
;;
;; 返回值
;; ----
;; pi-thinking-block?
;; 思考块 record 对象。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(let* ((blk (pi-thinking-block "Let me think...")))
  (check-true (pi-thinking-block? blk))
  (check-true (pi-block? blk))
  (check (pi-block-type blk) => "thinking")
  (check (pi-thinking-block-thinking blk) => "Let me think...")
  (check (pi-thinking-block-thinking-signature blk) => #f)
) ;let*

(let* ((blk (pi-thinking-block "Reasoning step" "sig-xyz-123")))
  (check-true (pi-thinking-block? blk))
  (check (pi-thinking-block-thinking blk) => "Reasoning step")
  (check (pi-thinking-block-thinking-signature blk) => "sig-xyz-123")
) ;let*

(check-report)
