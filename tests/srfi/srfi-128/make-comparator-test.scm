;; make-comparator
;; 创建比较器。
;;
;; 语法
;; ----
;; (make-comparator type-test equality ordering hash)
;;
;; 参数
;; ----
;; type-test : procedure 或 #t
;; 类型判定过程，接收一个对象返回布尔值；#t 表示接受任意对象。
;; equality : procedure 或 #t
;; 相等判定过程，接收两个对象返回布尔值；#t 表示由 ordering 推导相等。
;; ordering : procedure 或 #f
;; 排序判定过程，接收两个对象返回布尔值；#f 表示不支持排序。
;; hash : procedure 或 #f
;; 哈希过程，接收一个对象返回整数；#f 表示不支持哈希。
;; 返回值
;; ----
;; comparator
;; 返回新建的比较器。
;; 注意
;; ----
;; ordering 为 #f 时，比较器不支持排序；hash 为 #f 时不支持哈希。
;; 错误处理
;; ----
;; 调用不支持的操作时抛出错误。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(let ((comp (make-comparator number? = < number-hash)))
  (check-true (comparator? comp))
  (check-true (comparator-ordered? comp))
  (check-true (comparator-hashable? comp))
  (check (=? comp 1 1) => #t)
  (check (<? comp 1 2) => #t)
) ;let

;; type-test 为 #t 时接受任意对象
(let ((comp (make-comparator #t = < number-hash)))
  (check (comparator-test-type comp "str") => #t)
) ;let

;; equality 为 #t 时由 ordering 推导相等
(let ((comp (make-comparator number? #t < number-hash)))
  (check (=? comp 1 1) => #t)
  (check (=? comp 1 2) => #f)
) ;let

;; 不支持排序与哈希
(let ((comp (make-comparator number? = #f #f)))
  (check-false (comparator-ordered? comp))
  (check-false (comparator-hashable? comp))
) ;let

(check-report)
