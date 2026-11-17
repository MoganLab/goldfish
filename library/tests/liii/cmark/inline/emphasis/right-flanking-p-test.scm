;; right-flanking?
;; 判断长度为 run-len 的定界符串是否满足右翼（right-flanking）规则。
;;
;; 语法
;; ----
;; (right-flanking? str pos run-len)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; pos : integer
;; 起始位置。
;; run-len : integer
;; 定界符长度。
;;
;; 返回值
;; ----
;; boolean
;; 满足返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emphasis))

(check-set-mode! 'report-failed)

(check-true (right-flanking? "*foo*" 4 1))
(check-false (right-flanking? "*foo *" 5 1))

(check-report)
