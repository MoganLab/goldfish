;; previous-char-start
;; 回退查找 pos 前一个 UTF-8 字符的起始字节索引。
;;
;; 语法
;; ----
;; (previous-char-start str pos)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; pos : integer
;; 当前字节索引。
;;
;; 返回值
;; ----
;; integer
;; 返回前一个字符的起始索引。

(import (scheme base))
(import (liii check))
(import (liii cmark inline char))

(check-set-mode! 'report-failed)

(check (previous-char-start "a中b" 4) => 1)
(check (previous-char-start "abc" 1) => 0)

(check-report)
