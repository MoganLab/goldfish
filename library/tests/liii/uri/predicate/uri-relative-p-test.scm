;; uri-relative?
;; 检查是否为相对 URI（无 scheme）。
;;
;; 语法
;; ----
;; (uri-relative? uri-obj)
;;
;; 参数
;; ----
;; uri-obj : any
;;   要检查的 URI 对象。
;;
;; 返回值
;; ----
;; boolean?
;;   如果是相对 URI 返回 #t，否则返回 #f。

(import (liii check) (liii uri record) (liii uri predicate))

(check-set-mode! 'report-failed)

;; 相对 URI（无 scheme）

(define u1 (make-uri-record #f "" "/path" '() #f))
(check (uri-relative? u1) => #t)


;; 空 scheme 也视为相对 URI

(define u2 (make-uri-record "" "" "/path" '() #f))
(check (uri-relative? u2) => #t)


;; 绝对 URI（有 scheme）

(define u3 (make-uri-record "https" "example.com" "/" '() #f))
(check (uri-relative? u3) => #f)


(define u4 (make-uri-record "http" "example.com" "/" '() #f))
(check (uri-relative? u4) => #f)


;; 非 URI 对象返回 #f
(check (uri-relative? "not-a-uri") => #f)
(check (uri-relative? 123) => #f)


(check-report)
