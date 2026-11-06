;; uri-scheme-raw
;; 读取 URI 记录中未经处理的 scheme 字段。
;;
;; 语法
;; ----
;; (uri-scheme-raw uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回构造记录时保存的 scheme 值。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define absolute (make-uri-raw "https" "example.com" "/" '() #f))

(define relative (make-uri-raw #f "" "file.txt" '() #f))
(check (uri-scheme-raw absolute) => "https")
(check (uri-scheme-raw relative) => #f)

(check-report)
