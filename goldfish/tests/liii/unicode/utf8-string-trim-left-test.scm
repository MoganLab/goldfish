;; utf8-string-trim-left
;; 从字符串左侧移除 Unicode 空白字符。
;;
;; 语法
;; ----
;; (utf8-string-trim-left str)
;;
;; 参数
;; ----
;; str : string?
;; 待去除左侧空白的字符串。
;;
;; 返回值
;; ----
;; string?
;; 移除左侧空白后的字符串；右侧内容原样保留。
;;
;; 说明
;; ----
;; 空白字符按 Unicode 码点判断（空格、制表符、换行符、回车符）；
;; 逐码点前进，不会拆断中文等多字节字符。

(import (liii check) (liii unicode))

(check-set-mode! 'report-failed)

(check (utf8-string-trim-left "") => "")
(check (utf8-string-trim-left "abc") => "abc")
(check (utf8-string-trim-left "  abc  ") => "abc  ")
(check (utf8-string-trim-left (string-append (string #\tab #\newline #\return) "x"))
  =>
  "x"
) ;check

;; 混合空白字符
(check (utf8-string-trim-left (string-append (string #\space #\tab #\newline) "y"))
  =>
  "y"
) ;check

;; 中文内容不被拆断
(check (utf8-string-trim-left "  汉字书写") => "汉字书写")

;; 全空白字符串被清空
(check (utf8-string-trim-left "   ") => "")

;; 非字符串参数抛出 type-error
(check-catch 'type-error (utf8-string-trim-left 123))

(check-report)
