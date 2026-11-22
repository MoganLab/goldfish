;; astral-char
;; 固化 astral 字符（U+10000..U+10FFFF）的支持边界：char 层完整支持
;; Unicode 码点，string 层按字节字符计数不默认 Unicode。
;;
;; 语法
;; ----
;; (import (scheme base) (liii unicode)) 后直接使用 char/string 原语。
;;
;; 参数
;; ----
;; 无：本文件为行为固化测试。
;;
;; 返回值
;; ----
;; 无：通过 check 断言验证行为。
;;
;; 设计决策（#1290）：
;;
;; 1. char 层完整支持 Unicode 码点：#\x10F700 等可直接构造，
;;    char->integer / integer->char / char=? / char<? 语义与 BMP 字符一致。
;;
;; 2. string 层不默认 Unicode：string 是字节字符序列，其内容可能来自
;;    任意编码（UTF-8、GBK、Latin-1……）的外部数据，运行时无法假设编码，
;;    因此 string-length / string-ref / string->list 一律按字节字符计数，
;;    这同时保证了 O(1) 随机访问等基础操作的性能：若默认按码点计数，
;;    每次索引都需从头打描变长编码。
;;    astral 字符进入 string 时按其 UTF-8 编码展开为 4 个字节字符。
;;    Unicode 本身也有多种编码形式（UTF-8/UTF-16BE/UTF-16LE），按码点
;;    操作时应使用 (liii unicode) 的 utf8-* / utf16be-* / utf16le-* 系列
;;    显式声明编码。
;;
;; 3. 文本端口的 read-char/peek-char 按字节字符读取；write-char 写入
;;    astral 字符时按 UTF-8 编码写入全部字节。

(import (liii check))
(import (scheme base))
(import (liii unicode))

(check-set-mode! 'report-failed)

;; ---- char 层：完整 Unicode 码点支持 ----

(check (char->integer #\x10F700) => 1111808)
(check (integer->char 1111808) => #\x10F700)
(check (char->integer (integer->char 1114111)) => 1114111)
(check (char? #\x10F700) => #t)
(check (char=? #\x10F700 #\x10F700) => #t)
(check (char<? #\x10F700 #\x10F701) => #t)
(check (char>? #\x10FFFF #\x10FFFE) => #t)

;; ---- string 层：按字节字符计数，astral 字符展开为 UTF-8 字节 ----

;; 每个 astral 字符占 4 个字节字符
(check (string-length (string #\x10F700 #\x10F701)) => 8)
(check (string-length (string #\x1F600)) => 4)

;; string-ref 返回 UTF-8 首字节对应的字符
(check (string-ref (string #\x10F700) 0) => #\xf4)
(check (string-ref (string #\a #\x10F700 #\b) 1) => #\xf4)

;; string->list 得到字节字符列表
(check (string->list (string #\x10F700 #\a))
  =>
  (list #\xf4 #\x8f #\x9c #\x80 #\a)
) ;check

;; ---- 文本端口：read-char/peek-char 按字节字符，write-char 按 UTF-8 写入 ----

(let ((p (open-input-string (string #\x10F700))))
  (check (peek-char p) => #\xf4)
  (check (peek-char p) => #\xf4)
  (check (read-char p) => #\xf4)
  (check (read-char p) => #\x8f)
  (check (read-char p) => #\x9c)
  (check (read-char p) => #\x80)
  (check (eof-object? (read-char p)) => #t)
) ;let

(let ((out (open-output-string)))
  (write-char #\x10F700 out)
  (let ((s (get-output-string out)))
    (check (string-length s) => 4)
    (check (utf8-string-length s) => 1)
    (check (utf8-substring s 0 1) => (string #\x10F700))
  ) ;let
) ;let

;; ---- (liii unicode)：按码点的 utf8-* 操作往返一致 ----

(check (utf8-string-length (string #\x10F700 #\x10F701)) => 2)
(check (utf8->string (string->utf8 (string #\x10F700 #\x1F600)))
  =>
  (string #\x10F700 #\x1F600)
) ;check
(check (utf8-substring (string #\x10F700 #\x10F701 #\x61) 0 2)
  =>
  (string #\x10F700 #\x10F701)
) ;check

(check-report)
