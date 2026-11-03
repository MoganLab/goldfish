;; pi-thinking-block-thinking
;; 获取思考内容块 record 的 thinking 字段。
;;
;; 语法
;; ----
;; (pi-thinking-block-thinking obj)
;;
;; 参数
;; ----
;; obj : any?
;; 思考内容块 record 实例。
;;
;; 返回值
;; ----
;; string?
;; thinking 字段的值。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-thinking-block "先分析需求" "sig-001"))
       (obj-b (%make-pi-thinking-block "再动手实现" #f))
      ) ;
  (check (pi-thinking-block-thinking obj-a) => "先分析需求")
  (check (pi-thinking-block-thinking obj-b) => "再动手实现")
) ;let*

(check-report)
