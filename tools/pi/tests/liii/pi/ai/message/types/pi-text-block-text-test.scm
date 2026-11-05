;; pi-text-block-text
;; 获取文本内容块 record 的 text 字段。
;;
;; 语法
;; ----
;; (pi-text-block-text obj)
;;
;; 参数
;; ----
;; obj : any?
;; 文本内容块 record 实例。
;;
;; 返回值
;; ----
;; string?
;; text 字段的值。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-text-block "Hello" #f)) (obj-b (%make-pi-text-block "World" #f)))
  (check (pi-text-block-text obj-a) => "Hello")
  (check (pi-text-block-text obj-b) => "World")
) ;let*

(check-report)
