(import (liii list) (liii check) (only (srfi srfi-1) delete-duplicates))


(check-set-mode! 'report-failed)


;; delete-duplicates 函数测试
;;
;; 语法
;; ----
;; (delete-duplicates list [=])
;;
;; 参数
;; ----
;; list : list?
;; 要处理的列表。
;;
;; = : procedure? (可选)
;; 比较函数，默认为equal?。
;;
;; 返回值
;; ------
;; list
;; 返回删除重复元素后的新列表，只保留第一次出现的元素。
;;
;; 示例
;; ----
;; (delete-duplicates (list 1 1 2 3)) => (list 1 2 3)
;; (delete-duplicates (list 1 2 3)) => (list 1 2 3)
;; (delete-duplicates (list 1 1 1)) => (list 1)
;; (delete-duplicates '(1 -2 3 2 -1) (lambda (x y) (= (abs x) (abs y)))) => (list 1 -2 3)


(check (delete-duplicates (list 1 1 2 3)) => (list 1 2 3))
(check (delete-duplicates (list 1 2 3)) => (list 1 2 3))
(check (delete-duplicates (list 1 1 1)) => (list 1))


(check (delete-duplicates (list)) => (list))


(check (delete-duplicates (list 1 1 2 3) (lambda (x y) #f)) => (list 1 1 2 3))


(check (delete-duplicates '(1 -2 3 2 -1) (lambda (x y) (= (abs x) (abs y))))
  =>
  (list 1 -2 3)
) ;check


(check (delete-duplicates '("a" "b" "a") string=?) => '("a" "b"))
(check (delete-duplicates '(#\a #\b #\a) char=?) => '(#\a #\b))
(check (delete-duplicates '(1 2 1 3 2) =) => '(1 2 3))
(check (delete-duplicates '(a b a c b) eq?) => '(a b c))
(check (delete-duplicates '((1) (2) (1)) equal?) => '((1) (2)))
(check (delete-duplicates '(#f #f #t #f)) => '(#f #t))

;; 大列表去重（哈希表优化应在毫秒级完成）
(check (length (delete-duplicates (append (iota 10000) (iota 10000)))) => 10000)

(check-catch 'type-error (delete-duplicates (list 1 1 2 3) 'not-pred))

;; 比较器与元素类型不匹配时，哈希路径下不匹配的元素静默保留（不报错）(devel/0156.md)
;; 注意：这是哈希优化带来的语义变化，修复前走 O(n^2) 扫描路径会抛 type-error
(check (delete-duplicates (list 1 "a" 1) =) => (list 1 "a"))
(check (delete-duplicates (list "a" 1 "a") string=?) => (list "a" 1))
(check (delete-duplicates (list #\a 1 #\a) char=?) => (list #\a 1))


(check-report)
