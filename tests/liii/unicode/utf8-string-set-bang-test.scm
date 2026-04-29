(import (liii check) (liii unicode) (scheme base))

(check-set-mode! 'report-failed)

;; utf8-string-set!
;; 按 Unicode 字符索引替换字符串中的字符，返回新字符串。
;;
;; 语法
;; ----
;; (utf8-string-set! str index char)
;;
;; 参数
;; ----
;; str : string?
;; 原始字符串。
;;
;; index : exact?
;; 必须是非负的精确整数，表示要替换的字符索引位置（按 Unicode 字符计）。
;;
;; char : char?
;; 新的字符值。
;;
;; 返回值
;; ------
;; string?
;; 替换后的新字符串。
;;
;; 说明
;; ----
;; 1. 索引按 Unicode 字符计算，而非字节数。
;; 2. 支持替换为不同字节长度的字符（如 ASCII 替换为中文字符）。
;; 3. 原字符串对象不会被修改。
;;
;; 错误处理
;; --------
;; out-of-range
;; 当索引为负数或超出字符串字符范围时抛出错误。
;; type-error
;; 当 str 不是字符串、index 不是整数、char 不是字符时抛出错误。
(check (utf8-string-set! "abc" 0 #\A) => "Abc")
(check (utf8-string-set! "abc" 1 #\B) => "aBc")
(check (utf8-string-set! "abc" 2 #\C) => "abC")
(check (utf8-string-set! "hello" 0 #\H) => "Hello")
(check (utf8-string-set! "hello" 4 #\O) => "hellO")
;; 替换为中文字符
(check (utf8-string-set! "abc" 1 #\中) => "a中c")
(check (utf8-string-set! "hello" 0 #\你) => "你ello")
(check (utf8-string-set! "中文字符" 1 #\英) => "中英字符")
;; 替换为 emoji
(check (utf8-string-set! "ab" 0 #\🚀) => "🚀b")
(check (utf8-string-set! "ab" 1 #\🚀) => "a🚀")
;; 单字符字符串
(check (utf8-string-set! "a" 0 #\b) => "b")
(check (utf8-string-set! "中" 0 #\英) => "英")
;; 多次替换
(let ((str "abc"))
  (check (utf8-string-set! str 0 #\X) => "Xbc")
  (check (utf8-string-set! str 1 #\Y) => "aYc")
  (check (utf8-string-set! str 2 #\Z) => "abZ")
  ;; 原字符串不变
  (check str => "abc")
) ;let
;; 错误处理测试
(check-catch 'type-error (utf8-string-set! 123 0 #\a))
(check-catch 'out-of-range (utf8-string-set! "abc" -1 #\a))
(check-catch 'out-of-range (utf8-string-set! "abc" 3 #\a))
(check-catch 'type-error (utf8-string-set! "abc" 0 "a"))
(check-catch 'out-of-range (utf8-string-set! "" 0 #\a))
(check-report)
