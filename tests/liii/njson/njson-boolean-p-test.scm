(import (liii check)
        (liii base)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-boolean?
;; 判断值是否表示 JSON boolean。
;;
;; 语法
;; ----
;; (njson-boolean? x)
;;
;; 参数
;; ----
;; x : any
;; njson 句柄或严格 JSON 标量。
;;
;; 返回值
;; ----
;; boolean?
;; 是 boolean 返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; `#t` 和 `#f` 都算 JSON boolean。
;;
;; 错误处理
;; ----
;; type-error
;; 非法句柄或已释放句柄时抛出。

(let-njson ((true-h (string->njson "true"))
            (false-h (string->njson "false"))
            (num-h (string->njson "1")))
  (check-true (njson-boolean? true-h))
  (check-true (njson-boolean? false-h))
  (check-false (njson-boolean? num-h))
) ;let-njson

(check-true (njson-boolean? #t))
(check-true (njson-boolean? #f))
(check-false (njson-boolean? 'foo))

(define njson-boolean-freed (string->njson "true"))
(check-true (njson-free njson-boolean-freed))
(check-catch 'type-error (njson-boolean? njson-boolean-freed))

(check-report)
