(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; string->flexvector
;; 字符串转换为可变长向量。
;;
;; 语法
;; ----
;; (string->flexvector str)
;;
(check (flexvector->vector (string->flexvector "abc")
       ) ;flexvector->vector
  =>
  #(#\a #\b #\c)
) ;check


(check-report)
