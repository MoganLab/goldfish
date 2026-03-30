(import (liii check)
        (liii base)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-null?
;; 判断值是否表示 JSON null。
;;
;; 语法
;; ----
;; (njson-null? x)
;;
;; 参数
;; ----
;; x : any
;; njson 句柄或严格 JSON 标量。
;;
;; 返回值
;; ----
;; boolean?
;; 是 null 返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 句柄和严格标量都支持判定。
;;
;; 错误处理
;; ----
;; type-error
;; 非法句柄或已释放句柄时抛出。

(let-njson ((null-h (string->njson "null"))
            (obj-h (string->njson "{\"x\":1}")))
  (check-true (njson-null? null-h))
  (check-false (njson-null? obj-h))
) ;let-njson

(check-true (njson-null? 'null))
(check-false (njson-null? 'foo))

(define njson-null-freed (string->njson "{\"k\":1}"))
(check-true (njson-free njson-null-freed))
(check-catch 'type-error (njson-null? njson-null-freed))

(check-report)
