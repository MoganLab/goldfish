(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; flexvector->string
;; 可变长向量转换为字符串。
;;
;; 语法
;; ----
;; (flexvector->string fv)
;;
(check (flexvector->string (flexvector #\a #\b #\c)
       ) ;flexvector->string
  =>
  "abc"
) ;check


(check-report)
