;; pi-image-block?
;; 判断对象是否为图片内容块 record 实例。
;;
;; 语法
;; ----
;; (pi-image-block? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于图片内容块 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-image-block "aGVsbG8=" "image/png")))
  (check-true (pi-image-block? obj))
) ;let
(check-false (pi-image-block? '()))
(check-false (pi-image-block? #f))
(check-false (pi-image-block? "not-a-record"))

(check-report)
