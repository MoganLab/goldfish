;; extension-registry?
;; 判断对象是否为扩展注册表记录。
;;
;; 语法
;; ----
;; (extension-registry? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 是扩展注册表记录时返回 #t，否则返回 #f。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(check-true (extension-registry? (make-extension-registry)))
(check-false (extension-registry? #f))
(check-false (extension-registry? '()))
(check-false (extension-registry? (list (cons 'tools '()))))

(check-report)
