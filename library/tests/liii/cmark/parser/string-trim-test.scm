(import (liii check) (liii cmark text) (liii unicode))

(check-set-mode! 'report-failed)

;; string-trim
;; 去除字符串两端空白，并配套验证左右 trim 与缩进计数工具。
;;
;; 语法
;; ----
;; (string-trim str)
;;
;; 描述
;; ----
;; string-trim-left、string-trim-right 与 count-leading-spaces
;; 通常一起参与块级行处理。

(check (string-trim-left "  hello") => "hello")
(check (string-trim-left "hello") => "hello")
(check (string-trim-left "") => "")
(check (string-trim-left "   ") => "")
(check (string-trim-left "\thello") => "hello")

(check (string-trim-right "hello  ") => "hello")
(check (string-trim-right "hello") => "hello")
(check (string-trim-right "") => "")
(check (string-trim-right "   ") => "")
(check (string-trim-right "hello\t") => "hello")

(check (string-trim "  hello  ") => "hello")
(check (string-trim "hello") => "hello")
(check (string-trim "") => "")
(check (string-trim "   ") => "")
(check (string-trim "\thello\t") => "hello")

;; 边界：非 ASCII 空白不修剪
;; NBSP（U+00A0）与全角空格（U+3000）不属于 ascii-whitespace?（codepoint 9-13 与空格 32），
;; string-trim* 一律保留（与旧 ASCII 实现一致，见 devel/1026.md）
;; 注：goldfish 的 string 只接受 U+0000..U+00FF，故 U+3000 需用 utf8-string 构造
(let ((nbsp (string (integer->char 160)))
      (fullwidth-space (utf8-string (integer->char 12288)))
     ) ;
  (check (string-trim (string-append nbsp "hello" nbsp))
    =>
    (string-append nbsp "hello" nbsp)
  ) ;check
  (check (string-trim-left (string-append nbsp "hello"))
    =>
    (string-append nbsp "hello")
  ) ;check
  (check (string-trim-right (string-append "hello" nbsp))
    =>
    (string-append "hello" nbsp)
  ) ;check
  (check (string-trim (string-append fullwidth-space "hello" fullwidth-space))
    =>
    (string-append fullwidth-space "hello" fullwidth-space)
  ) ;check
  ;; 混合：两侧 ASCII 空格被修剪，中间的 NBSP 保留
  (check (string-trim (string-append "  " nbsp "hello" nbsp "  "))
    =>
    (string-append nbsp "hello" nbsp)
  ) ;check
) ;let

;; 边界：\r（CR）与 \v（VT，U+000B）属于 ascii-whitespace?，会被修剪
(check (string-trim "\rhello\r") => "hello")
(check
  (string-trim (string-append (string (integer->char 11)) "hello" (string (integer->char 11)))
  ) ;string-trim
  =>
  "hello"
) ;check

(check (count-leading-spaces "    x") => 4)
(check (count-leading-spaces "x") => 0)
(check (count-leading-spaces "") => 0)
(check (count-leading-spaces "  x  ") => 2)
(check (count-leading-spaces "\tx") => 0)

(check-report)
