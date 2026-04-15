(import (liii check) (liii uri-record))


(check-set-mode! 'report-failed)


;; uri-suffix
;; 获取文件后缀。
;;
;; 语法
;; ----
;; (uri-suffix uri-obj)
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回文件扩展名，无后缀返回 #f。


;; 有后缀
(define u1
  (make-uri-raw "https"
    "example.com"
    "/file.txt"
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-suffix u1) => "txt")


;; 多后缀
(define u2
  (make-uri-raw "https"
    "example.com"
    "/file.tar.gz"
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-suffix u2) => "gz")


;; 无后缀
(define u3
  (make-uri-raw "https"
    "example.com"
    "/README"
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-suffix u3) => #f)


;; 目录路径
(define u4
  (make-uri-raw "https"
    "example.com"
    "/path/"
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-suffix u4) => #f)


(check-report)
