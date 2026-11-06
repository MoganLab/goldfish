;; uri-decode-path
;; 解码 URI 路径中的百分比编码。
;;
;; 语法
;; ----
;; (uri-decode-path path)
;;
;; 参数
;; ----
;; path : string
;;   要解码的路径字符串。
;;
;; 返回值
;; ----
;; string
;;   返回解码后的路径。
;;
;; 错误处理
;; ----
;; 参数不是字符串时抛出 type-error；非法百分比编码时抛出 error。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(check (uri-decode-path "/a%20b/c") => "/a b/c")
(check (uri-decode-path "/%E4%B8%AD%E6%96%87/file.txt") => "/中文/file.txt")
(check (uri-decode-path "/a+b") => "/a b")
(check (uri-decode-path "/a%2Fb") => "/a/b")
(check-catch 'type-error (uri-decode-path 123))
(check-catch #t (uri-decode-path "/bad%2"))

(check-report)
