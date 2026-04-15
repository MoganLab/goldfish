(import (liii check) (liii uri-record))


(check-set-mode! 'report-failed)


;; uri-parent
;; 获取父目录路径。
;;
;; 语法
;; ----
;; (uri-parent uri-obj)
;;
;; 返回值
;; ----
;; string?, #f, 或 ""
;;   返回父目录路径，根路径返回 #f，无目录返回 ""。


;; 多级路径
(define u1
  (make-uri-raw "https"
    "example.com"
    "/a/b/c.txt"
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-parent u1) => "/a/b")


;; 单级路径
(define u2
  (make-uri-raw "https"
    "example.com"
    "/file.txt"
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-parent u2) => "")


;; 根路径
(define u3
  (make-uri-raw "https"
    "example.com"
    "/"
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-parent u3) => #f)


;; 空路径
(define u4
  (make-uri-raw "https"
    "example.com"
    ""
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-parent u4) => #f)


(check-report)
