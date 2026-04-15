(import (liii check)
  (liii error)
  (liii either)
) ;import

(check-set-mode! 'report-failed)

;; either-for-each
;; 对 Right 值执行副作用函数，对 Left 值不做任何操作。
;;
;; 语法
;; ----
;; (either-for-each proc either)
;;
;; 参数
;; ----
;; proc : procedure?
;; 要作用于 Right 值的过程。
;;
;; either : either
;; 输入 Either 值。
;;
;; 返回值
;; ----
;; unspecified
;; 主要依赖副作用，不关注返回值。
;;
;; 注意
;; ----
;; 对 Left 值调用时不会执行 proc。
;;
;; 示例
;; ----
;; (either-for-each display (from-right 5)) => 对 5 执行 display
;;
;; 错误处理
;; ----
;; type-error 当 proc 不是过程或 either 不是 Either 时

(let ((counter 0)
      (left-val (from-left "error"))
      (right-val (from-right 5))
     ) ;
  (either-for-each (lambda (x)
                     (set! counter (+ counter x))
                   ) ;lambda
    left-val
  ) ;either-for-each
  (check counter => 0)
  (either-for-each (lambda (x)
                     (set! counter (+ counter x))
                   ) ;lambda
    right-val
  ) ;either-for-each
  (check counter => 5)
) ;let

(check-catch 'type-error
  (either-for-each (lambda (x) x)
    "not-either"
  ) ;either-for-each
) ;check-catch
(check-catch 'type-error
  (either-for-each "not-a-proc"
    (from-right 10)
  ) ;either-for-each
) ;check-catch

(check-report)
