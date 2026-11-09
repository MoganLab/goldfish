;; string%take-right
;; 取字符串尾部 n 个字符，越界宽容（对标 Scala String.takeRight）。
;;
;; 语法
;; ----
;; ($ s :take-right n)
;;
;; 参数
;; ----
;; n : integer?
;; 保留的尾部字符数，越界/负数宽容。
;;
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; string?
;; 尾部 n 个字符；UTF-8 语义（string-take-right 越界抛错，helper clamp）。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "abc" :take-right 2) => "bc")
(check ($ "abc" :take-right 5) => "abc")
(check ($ "测试法" :take-right 2) => "试法")
(check ($ "" :take-right 3) => "")

(check-report)
