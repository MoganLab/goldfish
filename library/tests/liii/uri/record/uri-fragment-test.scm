;; uri-fragment
;; 读取 URI 的片段标识符。
;;
;; 语法
;; ----
;; (uri-fragment uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回片段标识符，不含前导井号；没有片段时返回 #f。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "example.com" "/docs" '() "intro"))
(check (uri-fragment u) => "intro")
(check (uri-fragment (make-uri-raw "https" "example.com" "/docs" '() "")) => "")
(check (uri-fragment (make-uri-raw "https" "example.com" "/docs" '() #f)) => #f)

(check-report)
