;; uri-encode-path
;; 对 URI 路径进行 UTF-8 百分比编码，同时保留路径分隔斜杠。
;;
;; 语法
;; ----
;; (uri-encode-path path)
;;
;; 参数
;; ----
;; path : string
;;   要编码的路径字符串。
;;
;; 返回值
;; ----
;; string
;;   返回编码后的路径，斜杠保持不变。
;;
;; 错误处理
;; ----
;; 参数不是字符串时抛出 error。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(check (uri-encode-path "/a b/c") => "/a%20b/c")
(check (uri-encode-path "/中文/file.txt") => "/%E4%B8%AD%E6%96%87/file.txt")
(check (uri-encode-path "/a=b") => "/a%3Db")
(check (uri-encode-path "") => "")
(check-catch #t (uri-encode-path 123))

(check-report)
