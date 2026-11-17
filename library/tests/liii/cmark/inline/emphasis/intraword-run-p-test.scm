;; intraword-run?
;; 判断定界符串是否同时位于字母数字词内（intraword）。
;;
;; 语法
;; ----
;; (intraword-run? str pos run-len)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; pos : integer
;; 起始位置。
;; run-len : integer
;; 标记长度。
;;
;; 返回值
;; ----
;; boolean
;; 位于词内返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emphasis))

(check-set-mode! 'report-failed)

(check-true (intraword-run? "a_b_c" 1 1))
(check-false (intraword-run? " _b_" 1 1))

(check-report)
