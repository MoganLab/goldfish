(import (liii check)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; pi-image-block
;; 构造图片内容块并验证访问器、类型判定与序列化。

(let ((blk (pi-image-block "aGVsbG8=" "image/png")))
  (check-true (pi-image-block? blk))
  (check-true (pi-block? blk))
  (check (pi-block-type blk) => "image")
  (check (pi-image-block-data blk) => "aGVsbG8=")
  (check (pi-image-block-mime-type blk) => "image/png")
  (check-false (pi-text-block? blk))

  (let ((al (pi-block->alist blk)))
    (check (cdr (assoc "type" al)) => "image")
    (check (cdr (assoc "data" al)) => "aGVsbG8=")
    (check (cdr (assoc "mimeType" al)) => "image/png")
  ) ;let
) ;let

;; 非图片块判定为 #f
(check-false (pi-image-block? (pi-text-block "test")))
(check-false (pi-image-block? "string"))
(check-false (pi-image-block? 123))

(check-report)
