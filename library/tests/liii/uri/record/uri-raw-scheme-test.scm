;; uri-raw-scheme
;; 读取 URI 的原始 scheme 值。
;;
;; 语法
;; ----
;; (uri-raw-scheme uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回与 uri-scheme-raw 相同的值。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri-raw-scheme u) => "https")
(check (uri-raw-scheme u) => (uri-scheme-raw u))
(check (uri-raw-scheme (make-uri-raw #f "" "" '() #f)) => #f)

(check-report)
