;; utf8-string-trim-both
;; 从字符串两侧移除 Unicode 空白字符。
;;
;; 语法
;; ----
;; (utf8-string-trim-both str)
;;
;; 参数
;; ----
;; str : string?
;; 待去除两侧空白的字符串。
;;
;; 返回值
;; ----
;; string?
;; 移除两侧空白后的字符串。
;;
;; 说明
;; ----
;; 等价于先调用 utf8-string-trim-left 再调用
;; utf8-string-trim-right，空白判定见 utf8-string-trim-left。

(import (liii check) (liii unicode))

(check-set-mode! 'report-failed)

(check (utf8-string-trim-both "") => "")
(check (utf8-string-trim-both "abc") => "abc")
(check (utf8-string-trim-both "  abc  ") => "abc")
(check (utf8-string-trim-both (string-append (string #\newline) " x " (string #\tab)))
  =>
  "x"
) ;check

;; 多字节空白字符当前不参与裁剪
(check (utf8-string-trim-both (string-append (utf8-string #\x3000) "y" (string #\space))
       ) ;utf8-string-trim-both
  =>
  (string-append (utf8-string #\x3000) "y")
) ;check

;; 中文内容不被拆断
(check (utf8-string-trim-both "  汉字书写  ") => "汉字书写")

;; 全空白字符串被清空
(check (utf8-string-trim-both "   ") => "")

;; 非字符串参数抛出 type-error
(check-catch 'type-error (utf8-string-trim-both 3.14))

(check-report)
