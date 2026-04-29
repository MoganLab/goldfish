(import (liii check)
  (liii unicode)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


;; utf8-string
;; 将给定字符按 UTF-8 编码后拼接成字符串。
;;
;; 语法
;; ----
;; (utf8-string char ...)
;;
;; 参数
;; ----
;; char ... : char
;; 任意数量的字符。
;;
;; 返回值
;; ----
;; string
;; 由输入字符按 UTF-8 编码后组成的新字符串。
;;
;; 错误处理
;; ----
;; type-error 当参数不是字符时。


;; 基本测试
(check (utf8-string) => "")
(check (utf8-string #\a) => "a")
(check (utf8-string #\a #\b #\c) => "abc")


;; Unicode 字符测试
(check (utf8-string #\中) => "中")
(check (utf8-string #\中 #\文) => "中文")
(check (utf8-string #\🐟 #\中 #\文) => "🐟中文")


;; 字节长度测试
(check (string-length (utf8-string #\中)) => 3)
(check (string-length (utf8-string #\中 #\文)) => 6)
(check (string-length (utf8-string #\a #\中 #\文)) => 7)


;; 兼容性测试
(check (string->utf8 (utf8-string #\中 #\文))
  =>
  #u8(228 184 173 230 150 135)
) ;check
(check (utf8-string #\H #\e #\l #\l #\o #\space #\🐟)
  =>
  "Hello 🐟"
) ;check


;; 错误处理测试
(check-catch 'type-error
  (utf8-string 1)
) ;check-catch
(check-catch 'type-error
  (utf8-string #\a "b")
) ;check-catch


(check-report)
