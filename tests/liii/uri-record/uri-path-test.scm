(import (liii check)
        (liii uri-record)
) ;import

(check-set-mode! 'report-failed)

;; uri-path
;; 获取 URI 的 path。
;;
;; 语法
;; ----
;; (uri-path uri-obj)
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回路径字符串。

;; 简单路径
(define u1 (make-uri-raw "https" "example.com" "/path/to/file" '() #f))
(check (uri-path u1) => "/path/to/file")

;; 根路径
(define u2 (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri-path u2) => "/")

;; 空路径
(define u3 (make-uri-raw "https" "example.com" "" '() #f))
(check (uri-path u3) => "")

;; 带查询的路径
(define u4 (make-uri-raw "https" "example.com" "/api/v1" '(("key" . "val")) #f))
(check (uri-path u4) => "/api/v1")

(check-report)
