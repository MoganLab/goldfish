;; string%drop-right
;; 去掉字符串尾部 n 个字符，越界宽容（对标 Scala String.dropRight）。
;;
;; 语法
;; ----
;; ($ s :drop-right n)
;;
;; 参数
;; ----
;; n : integer?
;; 去掉的尾部字符数，越界/负数宽容。
;;
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; string?
;; 去掉尾部 n 个后的剩余；UTF-8 语义。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "abc" :drop-right 1) => "ab")
(check ($ "abc" :drop-right 10) => "")
(check ($ "测试法" :drop-right 1) => "测试")

(check-report)
