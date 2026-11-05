;; %make-pi-thinking-block
;; 构造思考内容块 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-thinking-block thinking thinking-signature)
;;
;; 参数
;; ----
;; thinking : string?
;; 思考过程文本
;;
;; thinking-signature : string? 或 #f
;; 服务商返回的思考签名，缺失时为 #f
;;
;; 返回值
;; ----
;; pi-thinking-block?
;; 思考内容块 record 实例。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-thinking-block "先分析需求" "sig-001")))
  (check-true (pi-thinking-block? obj))
  (check (pi-thinking-block-thinking obj) => "先分析需求")
  (check (pi-thinking-block-thinking-signature obj) => "sig-001")
) ;let

(check-report)
