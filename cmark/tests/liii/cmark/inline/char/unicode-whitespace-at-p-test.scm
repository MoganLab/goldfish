;; unicode-whitespace-at?
;; 判断 str 在 pos 处是否为 Unicode 空白字符。
;;
;; 语法
;; ----
;; (unicode-whitespace-at? str pos)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; pos : integer
;; 字节索引。
;;
;; 返回值
;; ----
;; boolean
;; 是空白返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline char))

(check-set-mode! 'report-failed)

(check-true (unicode-whitespace-at? " a" 0))
(check-false (unicode-whitespace-at? "a " 0))

(check-report)
