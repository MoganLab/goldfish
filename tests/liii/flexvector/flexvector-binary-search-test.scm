(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-binary-search
;; 在已排序的可变长向量中进行二分查找。
;;
;; 语法
;; ----
;; (flexvector-binary-search fv value cmp)
;; (flexvector-binary-search fv value cmp start)
;; (flexvector-binary-search fv value cmp start end)
;;
;; 参数
;; ----
;; fv : flexvector
;; 已排序的目标向量。
;;
;; value : any
;; 要查找的值。
;;
;; cmp : function
;; 比较函数，返回负数、零或正数。
;;
;; start : exact-nonnegative-integer (可选)
;; 起始索引，默认为 0。
;;
;; end : exact-nonnegative-integer (可选)
;; 结束索引，默认为向量长度。
;;
;; 返回值
;; ----
;; exact-nonnegative-integer 或 #f
;; 找到则返回索引，否则返回 #f。
;;
;; 描述
;; ----
;; 使用二分查找算法在已排序向量中查找元素。

(let ((fv (flexvector #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j))
      (cmp (lambda (char1 char2)
             (cond ((char<? char1 char2) -1)
                   ((char=? char1 char2) 0)
                   (else 1)))
             ) ;cond
      ) ;cmp
  (check (flexvector-binary-search fv #\d cmp) => 3)
  (check (flexvector-binary-search fv #\a cmp) => 0)
  (check (flexvector-binary-search fv #\j cmp) => 9)
  (check (flexvector-binary-search fv #\k cmp) => #f)
  (check (flexvector-binary-search fv #\f cmp 2 6) => 5)
  (check (flexvector-binary-search fv #\f cmp 1 5) => #f)
) ;let

(let ((fv (flexvector 1 3 5 7 9 11))
      (cmp -))
  (check (flexvector-binary-search fv 5 cmp) => 2)
  (check (flexvector-binary-search fv 6 cmp) => #f)
) ;let

(check-report)
