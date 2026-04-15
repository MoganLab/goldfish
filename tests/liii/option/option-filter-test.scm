(import (liii check) (liii option))


(check-set-mode! 'report-failed)


;; option-filter
;; 根据谓词函数过滤 option 中的值。
;;
;; 语法
;; ----
;; (option-filter pred opt)
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
;; option 对象：
;; - 如果原 option 为空：返回空 option
;; - 如果值满足条件：返回原 option
;; - 如果值不满足条件：返回空 option


(let ((opt1 (option 42))
      (opt2 (none))
      (opt3 (option -5))
     ) ;
  (check (option-filter (lambda (x) (> x 0))
           opt1
         ) ;option-filter
    =>
    (option 42)
  ) ;check
  (check (option-filter (lambda (x) (> x 100))
           opt1
         ) ;option-filter
    =>
    (none)
  ) ;check
  (check (option-filter (lambda (x) (> x 0))
           opt2
         ) ;option-filter
    =>
    (none)
  ) ;check
  (check (option-filter (lambda (x) (> x 0))
           opt3
         ) ;option-filter
    =>
    (none)
  ) ;check
) ;let


(check-report)
