(import (liii check) (liii string))

;; char-position
;; 在字符串中查找字符首次出现的位置。
;;
;; 语法
;; ----
;; (char-position char-or-charset str)
;; (char-position char-or-charset str start)
;;
;; 参数
;; ----
;; char-or-charset : char? 或 string?
;; 当为 char? 时，查找该字符的位置；当为 string? 时，查找字符集中任意字符首次出现的位置。
;;
;; str : string?
;; 被搜索的源字符串。
;;
;; start : integer? (可选，默认为 0)
;; 搜索的起始位置。
;;
;; 返回值
;; ----
;; integer? 或 #f
;; 如果找到则返回位置索引，否则返回 #f。
;;
;; 注意
;; ----
;; 这是 s7 内置函数，(liii string) 重新导出以便统一使用。
;; 与 string-position 不同，char-position 支持按字符查找。
;;
;; 局限性
;; ----
;; char-position 按字节匹配，不支持 Unicode 字符。查找多字节 UTF-8 字符时会返回 #f。

(check (char-position #\o "hello world") => 4)
(check (char-position #\z "hello world") => #f)
(check (char-position #\h "hello") => 0)
(check (char-position #\o "hello world" 5) => 7)
(check-true (integer? (char-position #\a "abc")))
(check (char-position #\你 "你好世界") => #f)

;; 第一个参数为 string? 时，查找字符集中任意字符首次出现的位置
(check (char-position "aeiou" "hello world") => 1)
(check (char-position "xyz" "hello world") => #f)

;; 边界情况：查找 #\null 字符
;; 若字符串中不包含 #\null，应返回 #f，而非返回末尾越界索引 (string-length str)
(check (char-position #\null "hello") => #f)
(check (char-position #\null "") => #f)

;; 非 ASCII Unicode 字符：码点超过 0xFF 时不应截断低 8 位匹配
;; U+0161 的低 8 位为 0x61 即 #\a，不能误匹配 "cat" 中的 #\a
(check (char-position #\š "cat") => #f)

(check-report)
