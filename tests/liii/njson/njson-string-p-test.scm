(import (liii check)
        (liii base)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-string?
;; 判断值是否表示 JSON string。
;;
;; 语法
;; ----
;; (njson-string? x)
;;
;; 参数
;; ----
;; x : any
;; njson 句柄或严格 JSON 标量。
;;
;; 返回值
;; ----
;; boolean?
;; 是 string 返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 对已释放句柄会抛 type-error。
;;
;; 错误处理
;; ----
;; type-error
;; 非法句柄或已释放句柄时抛出。

(let-njson ((str-h (string->njson "\"s\""))
            (num-h (string->njson "3.14"))
            (obj-h (string->njson "{\"x\":1}")))
  (check-true (njson-string? str-h))
  (check-false (njson-string? num-h))
  (check-false (njson-string? obj-h))
) ;let-njson

(check-true (njson-string? "hello"))
(check-false (njson-string? 'foo))

(define njson-string-freed-p (string->njson "\"x\""))
(check-true (njson-free njson-string-freed-p))
(check-catch 'type-error (njson-string? njson-string-freed-p))

(check-report)
