(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; ellipsis-identifier?
;; 判断给定标识符是否是标准省略号标识符 ... 。
;;
;; 语法
;; ----
;; (ellipsis-identifier? id)
;;
;; 参数
;; ----
;; id : any
;; 待检查的对象。
;;
;; 返回值
;; -----
;; boolean
;; 若 id 为标识符且符号名称为 ... 则返回 #t，否则返回 #f。

;; 1. 省略号标识符判定
(check-true (ellipsis-identifier? '...))

;; 2. 非省略号标识符判定
(check-false (ellipsis-identifier? '_))
(check-false (ellipsis-identifier? 'foo))
(check-false (ellipsis-identifier? 123))
(check-false (ellipsis-identifier? "..."))

(check-report)
