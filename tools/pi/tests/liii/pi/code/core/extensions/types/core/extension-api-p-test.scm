;; extension-api?
;; 判断对象是否为扩展 API 记录。
;;
;; 语法
;; ----
;; (extension-api? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 是扩展 API 记录时返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(check-true (extension-api? (make-extension-api)))
(check-false (extension-api? #f))
(check-false (extension-api? '()))
(check-false (extension-api? (make-extension-registry)))

(check-report)
