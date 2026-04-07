(import (liii check)
        (liii uri-record)
        (liii uri-compare)
) ;import

(check-set-mode! 'report-failed)

;; uri-hash
;; 计算 URI 的哈希值。
;;
;; 语法
;; ----
;; (uri-hash uri-obj)
;;
;; 返回值
;; ----
;; integer?
;;   返回 URI 的哈希值（基于各组件长度之和）。

;; 相同 URI 有相同哈希值
(define u1 (make-uri-raw "https" "example.com" "/" '() #f))
(define u2 (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri-hash u1) => (uri-hash u2))

;; 不同 scheme，不同哈希值
(define u3 (make-uri-raw "http" "example.com" "/" '() #f))
(check (= (uri-hash u1) (uri-hash u3)) => #f)

;; 计算具体值
;; scheme="https" (5) + netloc="example.com" (11) + path="/" (1) = 17
(check (uri-hash u1) => 17)

;; 带 query 的 URI
(define u4 (make-uri-raw "https" "example.com" "/" '(("a" . "1")) #f))
(check (> (uri-hash u4) (uri-hash u1)) => #t)

;; 带 fragment 的 URI
(define u5 (make-uri-raw "https" "example.com" "/" '() "section"))
(check (> (uri-hash u5) (uri-hash u1)) => #t)

;; 复杂 URI
(define u6 (make-uri-raw "https" "user@host:8080" "/path/to/file"
                        '(("key1" . "val1") ("key2" . "val2")) "frag")
) ;define
(check (number? (uri-hash u6)) => #t)
(check (>= (uri-hash u6) 0) => #t)

;; 空 URI
(define u7 (make-uri-raw #f "" "" '() #f))
(check (uri-hash u7) => 0)

(check-report)
