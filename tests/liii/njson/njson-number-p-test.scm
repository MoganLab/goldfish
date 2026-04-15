(import (liii check)
  (liii base)
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; njson-number?
;; 判断值是否表示 JSON number。
;;
;; 语法
;; ----
;; (njson-number? x)
;;
;; 参数
;; ----
;; x : any
;; njson 句柄或严格 JSON 标量。
;;
;; 返回值
;; ----
;; boolean?
;; 是 number 返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 整数和实数都算 number。
;;
;; 错误处理
;; ----
;; type-error
;; 非法句柄或已释放句柄时抛出。


(let-njson ((num-h (string->njson "3.14"))
            (int-h (string->njson "7"))
            (obj-h (string->njson "{\"x\":1}"))
           ) ;
  (check-true (njson-number? num-h))
  (check-true (njson-number? int-h))
  (check-false (njson-number? obj-h))
) ;let-njson


(check-true (njson-number? 3.14))
(check-true (njson-number? 7))
(check-false (njson-number? 'foo))


(define njson-number-freed
  (string->njson "1")
) ;define
(check-true (njson-free njson-number-freed)
) ;check-true
(check-catch 'type-error
  (njson-number? njson-number-freed)
) ;check-catch


(check-report)
