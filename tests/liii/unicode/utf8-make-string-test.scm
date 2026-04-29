(import (liii check)
        (liii unicode)
        (scheme base)
) ;import

(check-set-mode! 'report-failed)

;; utf8-make-string
;; 创建一个由指定 UTF-8 字符重复填充的新字符串。
;;
;; 语法
;; ----
;; (utf8-make-string k [char])
;;
;; 参数
;; ----
;; k : exact?
;; 必须是非负的精确整数，表示要创建的字符串长度（按字符数计）。
;;
;; char : char? 可选
;; 用于填充字符串的字符。若未提供，默认使用 #\space。
;;
;; 返回值
;; ------
;; string?
;; 新创建的字符串，长度为 k，所有字符均为 char（或默认空格）。
;;
;; 说明
;; ----
;; 1. 与 make-string 不同，utf8-make-string 支持 Unicode 字符作为填充字符。
;; 2. 当 k 为 0 时返回空字符串 ""。
;; 3. 字符数按 Unicode 字符计算，而非字节数。
;;
;; 错误处理
;; --------
;; out-of-range
;; 当 k 为负数时抛出错误。
;; type-error
;; 当提供的填充参数不是字符时抛出错误。
(check (utf8-make-string 0) => "")
(check (utf8-make-string 0 #\a) => "")
(check (utf8-make-string 0 #\中) => "")
(check (utf8-make-string 1 #\a) => "a")
(check (utf8-make-string 3 #\a) => "aaa")
(check (utf8-string-length (utf8-make-string 3 #\a)) => 3)
(check (utf8-make-string 1 #\中) => "中")
(check (utf8-make-string 3 #\中) => "中中中")
(check (utf8-string-length (utf8-make-string 3 #\中)) => 3)
(check (utf8-make-string 2 #\🚀) => "🚀🚀")
(check (utf8-string-length (utf8-make-string 2 #\🚀)) => 2)
(let ((str (utf8-make-string 5 #\space)))
  (check (utf8-string-length str) => 5)
  (check (string-ref str 0) => #\space)
) ;let
(let ((str (utf8-make-string 5)))
  (check (utf8-string-length str) => 5)
  (check (string-ref str 0) => #\space)
) ;let
(check-catch 'out-of-range
  (utf8-make-string -1 #\a)
) ;check-catch
(check-catch 'out-of-range
  (utf8-make-string -1)
) ;check-catch
(check-catch 'type-error
  (utf8-make-string 3 "a")
) ;check-catch
(check-report)
