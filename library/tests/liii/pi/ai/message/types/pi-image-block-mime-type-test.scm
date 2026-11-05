;; pi-image-block-mime-type
;; 获取图片内容块中的 MIME 类型字符串。
;;
;; 语法
;; ----
;; (pi-image-block-mime-type block)
;;
;; 参数
;; ----
;; block : pi-image-block?
;; 图片内容块对象。
;;
;; 返回值
;; ----
;; string?
;; 图片 MIME 类型（如 "image/png"）。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-image-block "aGVsbG8=" "image/png")))
  (check (pi-image-block-mime-type obj) => "image/png")
) ;let

(check-report)
