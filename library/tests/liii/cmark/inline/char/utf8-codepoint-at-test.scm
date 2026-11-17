;; utf8-codepoint-at
;; 解码 str 在 pos 处的 UTF-8 码点，返回 (codepoint . byte-len)。
;;
;; 语法
;; ----
;; (utf8-codepoint-at str pos)
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
;; pair 或 #f
;; 合法 UTF-8 返回点对，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark inline char))

(check-set-mode! 'report-failed)

(check (utf8-codepoint-at "A" 0) => '(65 . 1))
(check (utf8-codepoint-at "中" 0) => '(20013 . 3))

(check-report)
