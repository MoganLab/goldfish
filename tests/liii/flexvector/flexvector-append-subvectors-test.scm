(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-append-subvectors
;; 连接多个可变长向量的子区间。
;;
;; 语法
;; ----
;; (flexvector-append-subvectors fv1 start1 end1 fv2 start2 end2 ...)
;;
;; 参数
;; ----
;; fv1, fv2, ... : flexvector
;; 源向量。
;;
;; start1, start2, ... : exact-nonnegative-integer
;; 各向量的起始索引。
;;
;; end1, end2, ... : exact-nonnegative-integer
;; 各向量的结束索引。
;;
;; 返回值
;; ----
;; flexvector
;; 包含所有子区间元素的新向量。
;;
;; 描述
;; ----
;; 从多个向量中提取子区间并连接成一个新向量。

(check (flexvector->vector
         (flexvector-append-subvectors
           (flexvector 'a 'b 'c 'd 'e) 0 2
           (flexvector 'f 'g 'h 'i 'j) 2 4)
         ) ;flexvector-append-subvectors
       => #(a b h i)
) ;check

(check (flexvector->vector
         (flexvector-append-subvectors (flexvector 1 2 3) 0 3))
       => #(1 2 3)
) ;check

(check-report)
