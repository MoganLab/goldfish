;; %make-pi-image-block
;; 构造图片内容块 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-image-block data mime-type)
;;
;; 参数
;; ----
;; data : string?
;; 图片 Base64 编码数据。
;; mime-type : string?
;; 图片 MIME 类型。
;;
;; 返回值
;; ----
;; pi-image-block?
;; 图片内容块 record 实例。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-image-block "aGVsbG8=" "image/png")))
  (check-true (pi-image-block? obj))
  (check (pi-image-block-data obj) => "aGVsbG8=")
  (check (pi-image-block-mime-type obj) => "image/png")
) ;let

(check-report)
