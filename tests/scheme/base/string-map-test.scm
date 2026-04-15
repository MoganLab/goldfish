(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string-map
;; 对字符串中的每个字符应用过程，返回结果字符串。
;;
;; 语法
;; ----
;; (string-map proc str1 str2 ...)
;;
;; 参数
;; ----
;; proc : procedure?
;; 接受字符作为参数的过程。
;;
;; str1, str2, ... : string?
;; 输入字符串。
;;
;; 返回值
;; ------
;; string?
;; 返回包含结果的字符串。
;; 基本测试
(check (string-map char-upcase "hello")
  =>
  "HELLO"
) ;check
(check (string-map char-downcase "WORLD")
  =>
  "world"
) ;check
;; 转换测试
(check (string-map (lambda (c)
                     (integer->char (+ 1 (char->integer c)))
                   ) ;lambda
         "abc"
       ) ;string-map
  =>
  "bcd"
) ;check
;; 空字符串测试
(check (string-map char-upcase "")
  =>
  ""
) ;check
;; 单字符测试
(check (string-map char-upcase "x")
  =>
  "X"
) ;check
;; 中文字符测试
(check (string-map (lambda (c) c) "你好")
  =>
  "你好"
) ;check
;; 多个字符串参数（如果支持）
(check-report)