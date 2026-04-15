(import (liii check)
  (liii base)
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; njson-integer?
;; 判断值是否表示 JSON integer。
;;
;; 语法
;; ----
;; (njson-integer? x)
;;
;; 参数
;; ----
;; x : any
;; njson 句柄或严格 JSON 标量。
;;
;; 返回值
;; ----
;; boolean?
;; 是整数返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 这里的整数指 JSON 整数值。
;;
;; 错误处理
;; ----
;; type-error
;; 非法句柄或已释放句柄时抛出。


(let-njson ((int-h (string->njson "7"))
            (num-h (string->njson "3.14"))
            (obj-h (string->njson "{\"x\":1}"))
           ) ;
  (check-true (njson-integer? int-h))
  (check-false (njson-integer? num-h))
  (check-false (njson-integer? obj-h))
) ;let-njson


(check-true (njson-integer? 7))
(check-false (njson-integer? 3.14))
(check-false (njson-integer? 'foo))


(define njson-integer-freed
  (string->njson "1")
) ;define
(check-true (njson-free njson-integer-freed)
) ;check-true
(check-catch 'type-error
  (njson-integer? njson-integer-freed)
) ;check-catch


(check-report)
