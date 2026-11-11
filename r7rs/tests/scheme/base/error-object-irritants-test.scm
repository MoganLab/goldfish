;; error-object-irritants
;; 返回错误对象的刺激物（irritants）列表。
;;
;;
;; 语法
;; ----
;; (error-object-irritants error-object)
;;
;; 参数
;; ----
;; error-object : error-object?
;; 待提取刺激物列表的错误对象。
;;
;; 返回值
;; ----
;; list?
;; 错误对象中包含的相关对象列表。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(define err1 (guard (ex (else ex)) (error "msg" 1 2 3)))
(define err2 (guard (ex (else ex)) (error "msg2")))

(check (error-object-irritants err1) => '(1 2 3))
(check (error-object-irritants err2) => '())

(check-report)
