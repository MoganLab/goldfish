(import (liii check)
  (liii uri-record)
  (liii uri-transform)
) ;import


(check-set-mode! 'report-failed)


;; uri-with-host
;; 替换 URI 的 host。
;;
;; 语法
;; ----
;; (uri-with-host uri-obj new-host)
;;
;; 返回值
;; ----
;; uri?
;;   返回新的 URI 记录。


;; 修改 host
(define u1
  (make-uri-raw "https"
    "old.com"
    "/"
    '()
    #f
  ) ;make-uri-raw
) ;define
(define u2 (uri-with-host u1 "new.com"))
(check (uri-host u2) => "new.com")
(check (uri-scheme u2) => "https")
(check (uri-path u2) => "/")


;; 保留其他 netloc 组件
(define u3
  (make-uri-raw "https"
    "user:pass@old.com:8080"
    "/"
    '()
    #f
  ) ;make-uri-raw
) ;define
(define u4 (uri-with-host u3 "new.com"))
(check (uri-user u4) => "user")
(check (uri-password u4) => "pass")
(check (uri-host u4) => "new.com")
(check (uri-explicit-port u4) => 8080)


(check-report)
