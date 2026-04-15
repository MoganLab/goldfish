(import (liii check) (liii uri-record))


(check-set-mode! 'report-failed)


;; uri-encode
;; 对字符串进行百分比编码。
;;
;; 语法
;; ----
;; (uri-encode str)
;;
;; 返回值
;; ----
;; string?
;;   返回编码后的字符串。


;; 无需编码
(check (uri-encode "hello") => "hello")
(check (uri-encode "test-file.txt")
  =>
  "test-file.txt"
) ;check


;; 空格编码
(check (uri-encode "hello world")
  =>
  "hello%20world"
) ;check


;; 特殊字符编码
(check (uri-encode "a=b") => "a%3Db")
(check (uri-encode "a&b") => "a%26b")


;; 中文字符编码（UTF-8）
(check (uri-encode "中文")
  =>
  "%E4%B8%AD%E6%96%87"
) ;check
(check (uri-encode "中")
  =>
  "%E4%B8%AD"
) ;check


;; 错误处理
(check-catch 'type-error
  (uri-encode 123)
) ;check-catch
(check-catch 'type-error
  (uri-encode '())
) ;check-catch


(check-report)
