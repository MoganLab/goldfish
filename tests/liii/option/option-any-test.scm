(import (liii check) (liii option))


(check-set-mode! 'report-failed)


;; option-any
;; 检查 option 中的值是否满足谓词函数（存在量词）。
;;
;; 语法
;; ----
;; (option-any pred opt)
;;
;; 参数
;; ----
;; pred : procedure
;; 接受一个参数的谓词函数，返回布尔值。
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; 布尔值：
;; - #f 如果 option 为空
;; - (pred value) 的结果如果 option 非空


(let ((opt1 (option 42))
      (opt2 (none))
      (opt3 (option -5))
     ) ;
  (check (option-any (lambda (x) (> x 0)) opt1)
    =>
    #t
  ) ;check
  (check (option-any (lambda (x) (> x 0)) opt2)
    =>
    #f
  ) ;check
  (check (option-any (lambda (x) (> x 0)) opt3)
    =>
    #f
  ) ;check
  (check (option-any (lambda (x) (number? x))
           opt1
         ) ;option-any
    =>
    #t
  ) ;check
  (check (option-any (lambda (x) (string? x))
           opt1
         ) ;option-any
    =>
    #f
  ) ;check
) ;let


(check-report)
