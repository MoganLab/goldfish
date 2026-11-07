;; extension-context?
;; 判断对象是否为扩展上下文记录。
;;
;; 语法
;; ----
;; (extension-context? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 是扩展上下文记录时返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(check-true (extension-context? (make-extension-context)))
(check-false (extension-context? #f))
(check-false (extension-context? '()))
(check-false (extension-context? (make-extension-registry)))

(check-report)
