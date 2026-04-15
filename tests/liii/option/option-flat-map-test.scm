(import (liii check) (liii option))


(check-set-mode! 'report-failed)


;; option-flat-map
;; 对 option 中的值应用返回 option 的映射函数。
;;
;; 语法
;; ----
;; (option-flat-map f opt)
;;
;; 参数
;; ----
;; f : procedure
;; 接受一个参数并返回 option 的函数。
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; option 对象：
;; - 如果原 option 为空：返回空 option
;; - 如果原 option 非空：返回 (f value)


(let ((opt1 (option 42)) (opt2 (none)))
  (check (option-flat-map (lambda (x) (option (+ x 1)))
           opt1
         ) ;option-flat-map
    =>
    (option 43)
  ) ;check
  (check (option-flat-map (lambda (x) (option (+ x 1)))
           opt2
         ) ;option-flat-map
    =>
    (none)
  ) ;check
  (check (option-flat-map (lambda (x) (none))
           opt1
         ) ;option-flat-map
    =>
    (none)
  ) ;check
) ;let


(check-report)
