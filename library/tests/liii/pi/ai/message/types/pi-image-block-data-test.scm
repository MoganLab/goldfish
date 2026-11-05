;; pi-image-block-data
;; 获取图片内容块中的 Base64 编码数据字符串。
;;
;; 语法
;; ----
;; (pi-image-block-data block)
;;
;; 参数
;; ----
;; block : pi-image-block?
;; 图片内容块对象。
;;
;; 返回值
;; ----
;; string?
;; 图片 Base64 编码数据。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-image-block "aGVsbG8=" "image/png")))
  (check (pi-image-block-data obj) => "aGVsbG8=")
) ;let

(check-report)
