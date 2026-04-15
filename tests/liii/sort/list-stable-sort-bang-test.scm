(import (liii check) (liii sort))


(check-set-mode! 'report-failed)


;; list-stable-sort!
;; 对列表进行破坏性稳定排序。
;;
;; 语法
;; ----
;; (list-stable-sort! cmp lst)
;;
;; 参数
;; ----
;; cmp : procedure
;; 比较函数，接受两个参数，返回布尔值。
;;
;; lst : list
;; 要排序的列表。
;;
;; 返回值
;; ----
;; list
;; 排序后的列表（原地修改）。相等元素的相对顺序保持不变。
;;
;; 注意
;; ----
;; 这是一个破坏性操作，原列表会被修改。稳定排序保证相等元素的原始顺序。
;;
;; 示例
;; ----
;; (list-stable-sort! < '(1 5 1 0 -1 1 5 1 0 1 1 5 9 2 4 3 4 9)) => 稳定排序后的列表
;;
;; 错误处理
;; ----
;; 无


(check-true (list-sorted? <
              (list-stable-sort! <
                '(1 5 1 0 -1 1 5 1 0 1 1 5 9 2 4 3 4 9)
              ) ;list-stable-sort!
            ) ;list-sorted?
) ;check-true
(check-true (list-sorted? <
              (list-stable-sort! <
                '(9 7 5 3 2 8 6 4 1 4 6 8 9 7 5 3 5 9 7 9)
              ) ;list-stable-sort!
            ) ;list-sorted?
) ;check-true
(check-true (list-sorted? <
              (list-stable-sort! <
                '(3 1 4 1 5 9 2 6 5 3 5 5 9 2 6 9)
              ) ;list-stable-sort!
            ) ;list-sorted?
) ;check-true
(check-true (list-sorted? <
              (list-stable-sort! <
                '(0 -1 2 -2 0 -1 0 2 3 1 3)
              ) ;list-stable-sort!
            ) ;list-sorted?
) ;check-true
(check-true (list-sorted? <
              (list-stable-sort! <
                '(5 -3 0 2 1 -1 2 1 2 4 5 -3 0 5)
              ) ;list-stable-sort!
            ) ;list-sorted?
) ;check-true
(check-true (list-sorted? <
              (list-stable-sort! < '())
            ) ;list-sorted?
) ;check-true
(check-true (list-sorted? <
              (list-stable-sort! < '(42))
            ) ;list-sorted?
) ;check-true


(check-report)
