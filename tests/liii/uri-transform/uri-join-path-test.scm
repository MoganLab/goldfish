(import (liii check)
  (liii uri-record)
  (liii uri-transform)
) ;import


(check-set-mode! 'report-failed)


;; uri-join-path
;; 在 URI 的 path 后追加段。
;;
;; 语法
;; ----
;; (uri-join-path uri-obj . segments)
;;
;; 返回值
;; ----
;; uri?
;;   返回新的 URI 记录。


;; 追加单个段
(define u1
  (make-uri-raw "https"
    "api.com"
    "/v1"
    '()
    #f
  ) ;make-uri-raw
) ;define
(define u2 (uri-join-path u1 "users"))
(check (uri-path u2) => "/v1/users")


;; 追加多个段
(define u3
  (make-uri-raw "https"
    "api.com"
    "/"
    '()
    #f
  ) ;make-uri-raw
) ;define
(define u4
  (uri-join-path u3
    "api"
    "v2"
    "resources"
  ) ;uri-join-path
) ;define
(check (uri-path u4)
  =>
  "/api/v2/resources"
) ;check


;; path 以 / 结尾
(define u5
  (make-uri-raw "https"
    "api.com"
    "/v1/"
    '()
    #f
  ) ;make-uri-raw
) ;define
(define u6 (uri-join-path u5 "items"))
(check (uri-path u6) => "/v1/items")


;; 保留 query 和 fragment
(define u7
  (make-uri-raw "https"
    "api.com"
    "/v1"
    '(("k" . "v"))
    "frag"
  ) ;make-uri-raw
) ;define
(define u8
  (uri-join-path u7 "endpoint")
) ;define
(check (uri-path u8) => "/v1/endpoint")
(check (uri-query-ref u8 "k") => "v")
(check (uri-fragment u8) => "frag")


(check-report)
