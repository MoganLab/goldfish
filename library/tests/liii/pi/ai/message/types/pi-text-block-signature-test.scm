;; pi-text-block-signature
;; 获取文本内容块 record 的 signature 字段。
;;
;; 语法
;; ----
;; (pi-text-block-signature obj)
;;
;; 参数
;; ----
;; obj : any?
;; 文本内容块 record 实例。
;;
;; 返回值
;; ----
;; string | #f
;; signature 字段的值。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-text-block "Hello" #f))
       (obj-b (%make-pi-text-block "World" "sig-text-1"))
      ) ;let*
  (check (pi-text-block-signature obj-a) => #f)
  (check (pi-text-block-signature obj-b) => "sig-text-1")
) ;let*

(check-report)
