(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; bound-identifier=?
;; 判断两个标识符是否具有完全相同的名字和绑定身份（用于宏引入变量的遮蔽与冲突检测）。
;;
;; 语法
;; ----
;; (bound-identifier=? x y)
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
;; 若两者同名且具有相同绑定身份则返回 #t，否则返回 #f。

;; 1. 相同符号
(check-true (bound-identifier=? 'a 'a))
(check-true (bound-identifier=? 'foo 'foo))

;; 2. 不同符号
(check-false (bound-identifier=? 'a 'b))
(check-false (bound-identifier=? 'foo 'bar))

(check-report)
