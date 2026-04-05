(import (liii check)
        (liii uri-record)
        (liii uri-compare)
) ;import

(check-set-mode! 'report-failed)

;; uri=?
;; 检查两个 URI 是否相等。
;;
;; 语法
;; ----
;; (uri=? uri1 uri2)
;;
;; 返回值
;; ----
;; boolean?
;;   如果两个 URI 相等返回 #t，否则返回 #f。

;; 完全相同的 URI
(define u1 (make-uri-raw "https" "example.com" "/" '() #f))
(define u2 (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri=? u1 u2) => #t)

;; 相同的 URI，不同的创建方式
(define u3 (make-uri-raw "http" "test.com" "/path" '(("a" . "1")) "frag"))
(define u4 (make-uri-raw "http" "test.com" "/path" '(("a" . "1")) "frag"))
(check (uri=? u3 u4) => #t)

;; 不同 scheme
(define u5 (make-uri-raw "http" "example.com" "/" '() #f))
(define u6 (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri=? u5 u6) => #f)

;; 不同 host
(define u7 (make-uri-raw "https" "a.com" "/" '() #f))
(define u8 (make-uri-raw "https" "b.com" "/" '() #f))
(check (uri=? u7 u8) => #f)

;; 不同 path
(define u9 (make-uri-raw "https" "example.com" "/a" '() #f))
(define u10 (make-uri-raw "https" "example.com" "/b" '() #f))
(check (uri=? u9 u10) => #f)

;; 与 #f 比较返回 #f
(check (uri=? u1 #f) => #f)
(check (uri=? #f u1) => #f)

(check-report)
