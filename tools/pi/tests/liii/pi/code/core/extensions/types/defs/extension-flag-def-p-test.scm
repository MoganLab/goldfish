;; extension-flag-def?
;; 判断对象是否为扩展标志定义记录。
;;
;; 语法
;; ----
;; (extension-flag-def? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 是标志定义记录时返回 #t，否则返回 #f。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(check-true (extension-flag-def? (make-extension-flag-def "--f" "d")))
(check-false (extension-flag-def? #f))
(check-false (extension-flag-def? '()))
(check-false (extension-flag-def? (list (cons 'flag "--f"))))

(check-report)
