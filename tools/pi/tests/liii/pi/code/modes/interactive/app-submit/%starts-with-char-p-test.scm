;; %starts-with-char?
;; 判定字符串首字符是否为指定字符。
;;
;; 语法
;; ----
;; (%starts-with-char? s c)
;;
;; 参数
;; ----
;; s : string?
;; 待检验的字符串。
;; c : char?
;; 目标前缀字符。
;;
;; 返回值
;; ----
;; boolean?
;; 满足条件返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi code modes interactive app-submit))

(check-set-mode! 'report-failed)

;; 1. 匹配首字符
(check-true (%starts-with-char? "!ls" #\!))
(check-true (%starts-with-char? "/model" #\/))

;; 2. 不匹配或空字符串
(check-false (%starts-with-char? "ls" #\!))
(check-false (%starts-with-char? "" #\!))
(check-false (%starts-with-char? " !ls" #\!))

(check-report)
