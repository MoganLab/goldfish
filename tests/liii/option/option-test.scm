(import (liii check) (liii option))


(check-set-mode! 'report-failed)


;; option
;; 创建包含值的 option 对象。
;;
;; 语法
;; ----
;; (option value)
;;
;; 参数
;; ----
;; value : any
;; 要包装的值。
;;
;; 返回值
;; -----
;; 包含值的 option 对象，内部表示为 (cons value 'S)。


(let ((opt1 (option 42))
      (opt2 (option "hello"))
      (opt3 (none))
     ) ;
  (check (option-defined? opt1) => #t)
  (check (option-defined? opt2) => #t)
  (check (option-empty? opt3) => #t)
  (check (option-defined? opt3) => #f)
) ;let


(check-report)
