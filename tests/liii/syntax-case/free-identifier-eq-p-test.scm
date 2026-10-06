(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; free-identifier=?
;; 判断两个标识符在各自绑定的使用环境中是否具有相同的有效词法/顶层绑定。
;;
;; 语法
;; ----
;; (free-identifier=? x y)
;;
;; 参数
;; ----
;; x : identifier
;; 第一个待比较的标识符。
;;
;; y : identifier
;; 第二个待比较的标识符。
;;
;; 返回值
;; -----
;; boolean
;; 若两者具有相同绑定则返回 #t，否则返回 #f。

;; 1. 相同符号标识符
(check-true (free-identifier=? 'a 'a))
(check-true (free-identifier=? 'lambda 'lambda))

;; 2. 不同符号标识符
(check-false (free-identifier=? 'a 'b))
(check-false (free-identifier=? 'car 'cdr))

(check-report)
