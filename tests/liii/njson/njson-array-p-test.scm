(import (liii check)
        (liii base)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-array?
;; 判断值是否表示 JSON array。
;;
;; 语法
;; ----
;; (njson-array? x)
;;
;; 参数
;; ----
;; x : any
;; njson 句柄或严格 JSON 标量。
;;
;; 返回值
;; ----
;; boolean?
;; 是 array 返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 对已释放句柄会抛 type-error。
;;
;; 错误处理
;; ----
;; type-error
;; 非法句柄或已释放句柄时抛出。

(let-njson ((arr-h (string->njson "[1,2]"))
            (obj-h (string->njson "{\"x\":1}"))
            (scalar-h (string->njson "1")))
  (check-true (njson-array? arr-h))
  (check-false (njson-array? obj-h))
  (check-false (njson-array? scalar-h))
) ;let-njson

(check-false (njson-array? 'null))
(check-false (njson-array? 1))

(define njson-array-freed (string->njson "[1]"))
(check-true (njson-free njson-array-freed))
(check-catch 'type-error (njson-array? njson-array-freed))

(check-report)
