;; flexvector-append-subvectors
;; 连接可变长向量的子向量。
;;
;; 语法
;; ----
;; (flexvector-append-subvectors fv1 start1 end1 fv2 start2 end2 ...)
;;
;; 参数
;; ----
;; fv : flexvector
;; 源可变长向量。
;;
;; start : exact-integer
;; 子向量起始索引（含）。
;;
;; end : exact-integer
;; 子向量结束索引（不含）。
;;
;; 返回值
;; ----
;; flexvector
;; 依次拼接各子向量得到的新可变长向量。

(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


(check (flexvector->vector (flexvector-append-subvectors (flexvector 'a 'b 'c 'd 'e)
                             0
                             2
                             (flexvector 'f 'g 'h 'i 'j)
                             2
                             4
                           ) ;flexvector-append-subvectors
       ) ;flexvector->vector
  =>
  #(a b h i)
) ;check


(check-report)
