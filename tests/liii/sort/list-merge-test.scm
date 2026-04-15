(import (liii check) (liii sort))


(check-set-mode! 'report-failed)


;; list-merge
;; 合并两个已排序的列表，返回一个新的已排序列表。
;;
;; 语法
;; ----
;; (list-merge cmp lst1 lst2)
;;
;; 参数
;; ----
;; cmp : procedure
;; 比较函数，接受两个参数，返回布尔值。
;;
;; lst1 : list
;; 第一个已排序列表。
;;
;; lst2 : list
;; 第二个已排序列表。
;;
;; 返回值
;; ----
;; list
;; 合并后的新列表，包含两个输入列表的所有元素且保持有序。
;;
;; 注意
;; ----
;; 这是一个非破坏性操作。两个输入列表必须已经按相同顺序排序。
;;
;; 示例
;; ----
;; (list-merge < '(1 3 5) '(2 4 6)) => '(1 2 3 4 5 6)
;;
;; 错误处理
;; ----
;; 无


;; 基本合并测试
(check (list-merge < '(1 3 5) '(2 4 6))
  =>
  '(1 2 3 4 5 6)
) ;check
(check (list-merge < '(1 1 3) '(1 2 4))
  =>
  '(1 1 1 2 3 4)
) ;check


;; 包含空列表的合并
(check (list-merge < '() '(1 2 3))
  =>
  '(1 2 3)
) ;check
(check (list-merge < '(1 2 3) '())
  =>
  '(1 2 3)
) ;check
(check (list-merge < '() '()) => '())


;; 使用 pair 比较函数
(define (pair-< x y)
  (< (car x) (car y))
) ;define


(define (pair-full-< x y)
  (cond ((not (= (car x) (car y)))
         (< (car x) (car y))
        ) ;
        (else (< (cdr y) (cdr x)))
  ) ;cond
) ;define


(check-true (list-sorted? pair-<
              (list-merge pair-<
                '((1 . 1) (1 . 2) (3 . 1))
                '((1 . 3) (2 . 1) (3 . 2) (4 . 1))
              ) ;list-merge
            ) ;list-sorted?
) ;check-true
(check (list-merge pair-<
         '((1 . 1) (1 . 2) (3 . 1))
         '((1 . 3) (2 . 1) (3 . 2) (4 . 1))
       ) ;list-merge
  =>
  '((1 . 1) (1 . 2) (1 . 3) (2 . 1) (3 . 1) (3 . 2) (4 . 1))
) ;check


(check-true (list-sorted? pair-full-<
              (list-merge pair-full-<
                '((1 . 2) (1 . 1) (3 . 1))
                '((1 . 3) (2 . 1) (3 . 2) (4 . 1))
              ) ;list-merge
            ) ;list-sorted?
) ;check-true
(check (list-merge pair-full-<
         '((1 . 2) (1 . 1) (3 . 1))
         '((1 . 3) (2 . 1) (3 . 2) (4 . 1))
       ) ;list-merge
  =>
  '((1 . 3) (1 . 2) (1 . 1) (2 . 1) (3 . 2) (3 . 1) (4 . 1))
) ;check


(check-report)
