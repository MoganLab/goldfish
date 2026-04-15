(import (liii check)
  (liii uri-record)
  (liii uri-predicate)
) ;import


(check-set-mode! 'report-failed)


;; uri-absolute?
;; 检查是否为绝对 URI（有 scheme）。
;;
;; 语法
;; ----
;; (uri-absolute? uri-obj)
;;
;; 返回值
;; ----
;; boolean?
;;   如果是绝对 URI 返回 #t，否则返回 #f。


;; 绝对 URI（有 scheme）
(define u1
  (make-uri-raw "https"
    "example.com"
    "/"
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-absolute? u1) => #t)


(define u2
  (make-uri-raw "http"
    "example.com"
    "/"
    '()
    #f
  ) ;make-uri-raw
) ;define
(check (uri-absolute? u2) => #t)


;; 相对 URI（无 scheme）
(define u3
  (make-uri-raw #f "" "/path" '() #f)
) ;define
(check (uri-absolute? u3) => #f)


;; 空 scheme
(define u4
  (make-uri-raw "" "" "/path" '() #f)
) ;define
(check (uri-absolute? u4) => #f)


;; 非 URI 对象返回 #f
(check (uri-absolute? "not-a-uri")
  =>
  #f
) ;check
(check (uri-absolute? 123) => #f)


(check-report)
