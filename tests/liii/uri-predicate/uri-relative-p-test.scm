(import (liii check)
        (liii uri-record)
        (liii uri-predicate)
) ;import

(check-set-mode! 'report-failed)

;; uri-relative?
;; 检查是否为相对 URI（无 scheme）。
;;
;; 语法
;; ----
;; (uri-relative? uri-obj)
;;
;; 返回值
;; ----
;; boolean?
;;   如果是相对 URI 返回 #t，否则返回 #f。

;; 相对 URI（无 scheme）
(define u1 (make-uri-raw #f "" "/path" '() #f))
(check (uri-relative? u1) => #t)

;; 空 scheme 也视为相对 URI
(define u2 (make-uri-raw "" "" "/path" '() #f))
(check (uri-relative? u2) => #t)

;; 绝对 URI（有 scheme）
(define u3 (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri-relative? u3) => #f)

(define u4 (make-uri-raw "http" "example.com" "/" '() #f))
(check (uri-relative? u4) => #f)

;; 非 URI 对象返回 #f
(check (uri-relative? "not-a-uri") => #f)
(check (uri-relative? 123) => #f)

(check-report)
