;; pi-image-block-type
;; 返回图片块的固定类型字符串 "image"。
;;
;; 语法
;; ----
;; (pi-image-block-type block)
;;
;; 参数
;; ----
;; block : any?
;; 图片块对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "image"。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(check (pi-image-block-type 'dummy) => "image")

(check-report)
