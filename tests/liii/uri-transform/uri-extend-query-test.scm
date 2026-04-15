(import (liii check)
  (liii uri-record)
  (liii uri-transform)
) ;import


(check-set-mode! 'report-failed)


;; uri-extend-query
;; 扩展 URI 的 query 参数。
;;
;; 语法
;; ----
;; (uri-extend-query uri-obj alist)
;;
;; 返回值
;; ----
;; uri?
;;   返回新的 URI 记录。


;; 添加 query 到空 query
(define u1
  (make-uri-raw "https"
    "api.com"
    "/"
    '()
    #f
  ) ;make-uri-raw
) ;define
(define u2
  (uri-extend-query u1 '(("page" . "1")))
) ;define
(check (uri-query-ref u2 "page") => "1")


;; 扩展现有 query
(define u3
  (make-uri-raw "https"
    "api.com"
    "/"
    '(("page" . "1"))
    #f
  ) ;make-uri-raw
) ;define
(define u4
  (uri-extend-query u3
    '(("limit" . "10"))
  ) ;uri-extend-query
) ;define
(check (uri-query-ref u4 "page") => "1")
(check (uri-query-ref u4 "limit")
  =>
  "10"
) ;check


;; 添加多个参数
(define u5
  (make-uri-raw "https"
    "api.com"
    "/"
    '()
    #f
  ) ;make-uri-raw
) ;define
(define u6
  (uri-extend-query u5
    '(("a" . "1") ("b" . "2") ("c" . "3"))
  ) ;uri-extend-query
) ;define
(check (uri-query-ref u6 "a") => "1")
(check (uri-query-ref u6 "b") => "2")
(check (uri-query-ref u6 "c") => "3")


;; 保留其他字段
(define u7
  (make-uri-raw "https"
    "api.com"
    "/path"
    '(("old" . "val"))
    "frag"
  ) ;make-uri-raw
) ;define
(define u8
  (uri-extend-query u7
    '(("new" . "item"))
  ) ;uri-extend-query
) ;define
(check (uri-path u8) => "/path")
(check (uri-query-ref u8 "old")
  =>
  "val"
) ;check
(check (uri-query-ref u8 "new")
  =>
  "item"
) ;check
(check (uri-fragment u8) => "frag")


(check-report)
