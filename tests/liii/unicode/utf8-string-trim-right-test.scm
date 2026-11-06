;; utf8-string-trim-right
;; 从字符串右侧移除 Unicode 空白字符。
;;
;; 语法
;; ----
;; (utf8-string-trim-right str)
;;
;; 参数
;; ----
;; str : string?
;; 待去除右侧空白的字符串。
;;
;; 返回值
;; ----
;; string?
;; 移除右侧空白后的字符串；左侧内容原样保留。
;;
;; 说明
;; ----
;; 空白字符按 Unicode 码点判断（空格、制表符、换行符、回车符）；
;; 逐码点回退，不会拆断中文等多字节字符。

(import (liii check) (liii unicode))

(check-set-mode! 'report-failed)

(check (utf8-string-trim-right "") => "")
(check (utf8-string-trim-right "abc") => "abc")
(check (utf8-string-trim-right "  abc  ") => "  abc")
(check (utf8-string-trim-right (string-append "x" (string #\tab #\newline)))
  =>
  "x"
) ;check

;; 混合空白字符
(check (utf8-string-trim-right (string-append "y" (string #\space #\tab #\newline)))
  =>
  "y"
) ;check

;; 中文内容不被拆断
(check (utf8-string-trim-right "汉字书写  ") => "汉字书写")

;; 全空白字符串被清空
(check (utf8-string-trim-right "   ") => "")

;; 非字符串参数抛出 type-error
(check-catch 'type-error (utf8-string-trim-right 'symbol))

(check-report)
