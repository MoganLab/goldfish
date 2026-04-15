(import (liii check) (liii uri-record))


(check-set-mode! 'report-failed)


;; uri?
;; 检查对象是否为 URI 记录。
;;
;; 语法
;; ----
;; (uri? obj)
;;
;; 参数
;; ----
;; obj : any
;;   任意对象。
;;
;; 返回值
;; ----
;; boolean?
;;   如果是 URI 记录返回 #t，否则返回 #f。


;; URI 对象
(check (uri? (make-uri-raw "http"
               "example.com"
               "/"
               '()
               #f
             ) ;make-uri-raw
       ) ;uri?
  =>
  #t
) ;check


;; 非 URI 对象
(check (uri? "string") => #f)
(check (uri? 123) => #f)
(check (uri? '()) => #f)
(check (uri? '(1 2 3)) => #f)
(check (uri? #t) => #f)


(check-report)
