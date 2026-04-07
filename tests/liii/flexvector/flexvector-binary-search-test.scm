(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-binary-search
;; 在有序 flexvector 中二分查找指定值。时间复杂度 O(log n)。
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
;;   有序源向量。
;;
;; value : any
;;   要查找的值。
;;
;; cmp : procedure
;;   比较函数，(cmp value element) 返回：
;;   - 负数：value < element
;;   - 0：value = element
;;   - 正数：value > element
;;
;; start : exact-nonnegative-integer (可选，默认 0)
;;   起始索引。
;;
;; end : exact-nonnegative-integer (可选，默认长度)
;;   结束索引（不包含）。
;;
;; 返回值
;; -----
;; 如果找到，返回元素的索引。
;; 如果没找到，返回 #f。
;;
;; 注意：fv 必须是有序的，否则结果未定义。
;;
;; 另见
;; ----
;; flexvector-index - 线性查找

;; 基本二分查找
(let ((fv (flexvector #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j))
      (cmp (lambda (char1 char2)
             (cond ((char<? char1 char2) -1)
                   ((char=? char1 char2) 0)
                   (else 1)))
             ) ;cond
      ) ;cmp
  ;; 查找不同位置
  (check (flexvector-binary-search fv #\d cmp) => 3)  ; 中间
  (check (flexvector-binary-search fv #\a cmp) => 0)  ; 开头
  (check (flexvector-binary-search fv #\j cmp) => 9)  ; 结尾
  ;; 没找到
  (check (flexvector-binary-search fv #\k cmp) => #f)
) ;let

;; 指定区间查找
(let ((fv (flexvector #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j))
      (cmp (lambda (char1 char2)
             (cond ((char<? char1 char2) -1)
                   ((char=? char1 char2) 0)
                   (else 1)))
             ) ;cond
      ) ;cmp
  ;; 在 [2, 6) 中查找
  (check (flexvector-binary-search fv #\f cmp 2 6) => 5)
  ;; 在 [1, 5) 中查找 #\f（不在范围内）
  (check (flexvector-binary-search fv #\f cmp 1 5) => #f)
) ;let

;; 数字查找
(let ((fv (flexvector 10 20 30 40 50)))
  (check (flexvector-binary-search fv 30 -) => 2)
  (check (flexvector-binary-search fv 10 -) => 0)
  (check (flexvector-binary-search fv 50 -) => 4)
  (check (flexvector-binary-search fv 25 -) => #f)
) ;let

;; 单元素
(let ((fv (flexvector 42)))
  (check (flexvector-binary-search fv 42 -) => 0)
  (check (flexvector-binary-search fv 0 -) => #f)
) ;let

;; 空向量
(check (flexvector-binary-search (flexvector) 'x (lambda (a b) 0)) => #f)

(check-report)
