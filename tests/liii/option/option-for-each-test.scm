(import (liii check) (liii option))


(check-set-mode! 'report-failed)


;; option-for-each
;; 对 option 中的值执行副作用操作。
;;
;; 语法
;; ----
;; (option-for-each f opt)
;;
;; 参数
;; ----
;; f : procedure
;; 接受一个参数的副作用函数。
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; 无（未定义）。
;;
;; 说明
;; ----
;; 如果 option 为空，则不执行任何操作。


(let ((opt1 (option 42))
      (opt2 (none))
      (result '())
     ) ;
  (option-for-each (lambda (x)
                     (set! result (cons x result))
                   ) ;lambda
    opt1
  ) ;option-for-each
  (check result => '(42))
  (set! result '())
  (option-for-each (lambda (x)
                     (set! result (cons x result))
                   ) ;lambda
    opt2
  ) ;option-for-each
  (check result => '())
) ;let


(check-report)
