;; pi-thinking-block-thinking-signature
;; 获取思考内容块 record 的 thinking-signature 字段。
;;
;; 语法
;; ----
;; (pi-thinking-block-thinking-signature obj)
;;
;; 参数
;; ----
;; obj : any?
;; 思考内容块 record 实例。
;;
;; 返回值
;; ----
;; string? 或 #f
;; thinking-signature 字段的值。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-thinking-block "先分析需求" "sig-001"))
       (obj-b (%make-pi-thinking-block "再动手实现" #f))
      ) ;
  (check (pi-thinking-block-thinking-signature obj-a) => "sig-001")
  (check (pi-thinking-block-thinking-signature obj-b) => #f)
) ;let*

(check-report)
