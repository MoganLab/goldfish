;; uri-raw-fragment
;; 读取 URI 记录中的原始片段标识符。
;;
;; 语法
;; ----
;; (uri-raw-fragment uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回与 uri-fragment-raw 相同的片段值。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "example.com" "/docs" '() "intro"))
(check (uri-raw-fragment u) => "intro")
(check (uri-raw-fragment u) => (uri-fragment-raw u))
(check (uri-raw-fragment (make-uri-raw "https" "example.com" "/" '() #f)) => #f)

(check-report)
