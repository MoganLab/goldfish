(import (liii check) (liii uri-record))


(check-set-mode! 'report-failed)


;; uri-scheme
;; 获取 URI 的 scheme。
;;
;; 语法
;; ----
;; (uri-scheme uri-obj)
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回 scheme 字符串，如 "http", "https"。


(define u1
  (make-uri-raw "https"
    "example.com"
    "/"
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-scheme u1) => "https")


(define u2
  (make-uri-raw "http"
    "example.com"
    "/"
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-scheme u2) => "http")


;; #f scheme
(define u3
  (make-uri-raw #f "" "/path" '() #f)
) ;define
(check (uri-scheme u3) => #f)


(check-report)
