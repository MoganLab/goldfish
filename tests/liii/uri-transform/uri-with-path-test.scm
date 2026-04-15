(import (liii check)
  (liii uri-record)
  (liii uri-transform)
) ;import


(check-set-mode! 'report-failed)


;; uri-with-path
;; 替换 URI 的 path。
;;
;; 语法
;; ----
;; (uri-with-path uri-obj new-path)
;;
;; 返回值
;; ----
;; uri?
;;   返回新的 URI 记录。


;; 修改 path
(define u1
  (make-uri-raw "https"
    "example.com"
    "/old"
    '()
    #f
  ) ;make-uri-raw
) ;define
(define u2 (uri-with-path u1 "/new"))
(check (uri-path u2) => "/new")
(check (uri-host u2) => "example.com")


;; 保留 query 和 fragment
(define u3
  (make-uri-raw "https"
    "api.com"
    "/v1"
    '(("k" . "v"))
    "frag"
  ) ;make-uri-raw
) ;define
(define u4 (uri-with-path u3 "/v2"))
(check (uri-path u4) => "/v2")
(check (uri-query-ref u4 "k") => "v")
(check (uri-fragment u4) => "frag")


(check-report)
