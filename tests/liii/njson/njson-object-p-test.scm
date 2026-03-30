(import (liii check)
        (liii base)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-object?
;; 判断值是否表示 JSON object。
;;
;; 语法
;; ----
;; (njson-object? x)
;;
;; 参数
;; ----
;; x : any
;; njson 句柄或严格 JSON 标量。
;;
;; 返回值
;; ----
;; boolean?
;; 是 object 返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 对已释放句柄会抛 type-error。
;;
;; 错误处理
;; ----
;; type-error
;; 非法句柄或已释放句柄时抛出。

(let-njson ((obj-h (string->njson "{\"x\":1}"))
            (arr-h (string->njson "[1,2]"))
            (null-h (string->njson "null")))
  (check-true (njson-object? obj-h))
  (check-false (njson-object? arr-h))
  (check-false (njson-object? null-h))
) ;let-njson

(check-false (njson-object? 'null))
(check-false (njson-object? 1))

(define njson-object-freed (string->njson "{\"k\":1}"))
(check-true (njson-free njson-object-freed))
(check-catch 'type-error (njson-object? njson-object-freed))

(check-report)
