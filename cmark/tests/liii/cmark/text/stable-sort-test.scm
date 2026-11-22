;; stable-sort
;; 按严格全序比较器 lt 对列表执行 O(n log n) 稳定归并排序。
;;
;; 语法
;; ----
;; (stable-sort xs lt)
;;
;; 参数
;; ----
;; xs : list
;; 待排序列表。
;; lt : procedure
;; 比较谓词，(lt a b) 为真表示 a 排在 b 前。
;;
;; 返回值
;; ----
;; list
;; 排序后的新列表，相等键保持原始相对顺序。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check (stable-sort '() <) => '())
(check (stable-sort '(7) <) => '(7))
(check (stable-sort '(5 3 1 4 2) <) => '(1 2 3 4 5))
(let ((result
        (stable-sort (list '(1 . a) '(1 . b) '(0 . c))
          (lambda (x y) (< (car x) (car y)))
        ) ;stable-sort
      ) ;result
     ) ;
  (check (equal? result (list '(0 . c) '(1 . a) '(1 . b))) => #t)
) ;let
(check (stable-sort '("b" "a" "c") string<?) => '("a" "b" "c"))
(check (stable-sort '(9 8 7 6 5 4 3 2 1) <) => '(1 2 3 4 5 6 7 8 9))

(check-report)
