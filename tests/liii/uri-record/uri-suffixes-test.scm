(import (liii check) (liii uri-record))


(check-set-mode! 'report-failed)


;; uri-suffixes
;; 获取所有后缀列表。
;;
;; 语法
;; ----
;; (uri-suffixes uri-obj)
;;
;; 返回值
;; ----
;; list?
;;   返回所有扩展名列表，无后缀返回空列表。


;; 单后缀
(define u1
  (make-uri-raw "https"
    "example.com"
    "/file.txt"
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-suffixes u1) => '("txt"))


;; 多后缀
(define u2
  (make-uri-raw "https"
    "example.com"
    "/file.tar.gz"
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-suffixes u2)
  =>
  '("tar" "gz")
) ;check


;; 无后缀
(define u3
  (make-uri-raw "https"
    "example.com"
    "/README"
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-suffixes u3) => '())


;; 多后缀（三个）
(define u4
  (make-uri-raw "https"
    "example.com"
    "/file.a.b.c"
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-suffixes u4)
  =>
  '("a" "b" "c")
) ;check


(check-report)
