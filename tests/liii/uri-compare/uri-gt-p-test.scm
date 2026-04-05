(import (liii check)
        (liii uri-record)
        (liii uri-compare)
) ;import

(check-set-mode! 'report-failed)

;; uri>?
;; 检查第一个 URI 是否大于第二个（字典序）。
;;
;; 语法
;; ----
;; (uri>? uri1 uri2)
;;
;; 返回值
;; ----
;; boolean?
;;   如果 uri1 > uri2 返回 #t，否则返回 #f。

;; 不同 scheme (https > http)
(define u1 (make-uri-raw "https" "example.com" "/" '() #f))
(define u2 (make-uri-raw "http" "example.com" "/" '() #f))
(check (uri>? u1 u2) => #t)
(check (uri>? u2 u1) => #f)

;; 相同 scheme，不同 host
(define u3 (make-uri-raw "http" "b.com" "/" '() #f))
(define u4 (make-uri-raw "http" "a.com" "/" '() #f))
(check (uri>? u3 u4) => #t)
(check (uri>? u4 u3) => #f)

;; 相同 scheme/host，不同 path
(define u5 (make-uri-raw "http" "example.com" "/b" '() #f))
(define u6 (make-uri-raw "http" "example.com" "/a" '() #f))
(check (uri>? u5 u6) => #t)
(check (uri>? u6 u5) => #f)

;; 相同 URI
(define u7 (make-uri-raw "https" "example.com" "/" '() #f))
(define u8 (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri>? u7 u8) => #f)

;; 与 #f 比较返回 #f
(check (uri>? u1 #f) => #f)
(check (uri>? #f u1) => #f)

(check-report)
