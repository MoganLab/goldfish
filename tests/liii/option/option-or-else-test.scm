(import (liii check) (liii option))


(check-set-mode! 'report-failed)


;; option-or-else
;; 如果当前 option 为空，则返回备选的 option。
;;
;; 语法
;; ----
;; (option-or-else alt opt)
;;
;; 参数
;; ----
;; alt : option
;; 备选 option。
;; opt : option
;; 当前 option 对象。
;;
;; 返回值
;; -----
;; 当前 option（如果非空）或备选 option（如果当前为空）。


(let ((opt1 (option 42))
      (opt2 (option 0))
      (opt3 (none))
     ) ;
  (check (option-or-else opt2 opt1)
    =>
    (option 42)
  ) ;check
  (check (option-or-else opt1 opt3)
    =>
    (option 42)
  ) ;check
  (check (option-or-else opt2 opt3)
    =>
    (option 0)
  ) ;check
  (check (option-or-else opt1 opt1)
    =>
    (option 42)
  ) ;check
) ;let


(check-report)
