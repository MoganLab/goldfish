(import (liii check) (liii enum))


(check-set-mode! 'report-failed)


(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define


(define color
  (make-enum-type color-names)
) ;define


(define color-set
  (enum-type->enum-set color)
) ;define


;; enum-set-for-each
;; 对集合中的每个成员执行过程。
;;
;; 语法
;; ----
;; (enum-set-for-each proc enum-set)
;;
;; 参数
;; ----
;; proc : procedure?
;; 要执行的过程。
;;
;; enum-set : enum-set?
;; 目标集合。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未指定。
;;
;; 注意
;; ----
;; 可通过副作用观察执行次数。
;;
;; 示例
;; ----
;; (let ((n 0)) (enum-set-for-each (lambda (_) (set! n (+ n 1))) color-set) n) => 8
;;
;; 错误处理
;; ----
;; 无。


(check (let ((n 0))
         (enum-set-for-each (lambda (_) (set! n (+ n 1)))
           color-set
         ) ;enum-set-for-each
         n
       ) ;let
  =>
  (length color-names)
) ;check


(check-report)
