(import (liii check)
        (liii error)
        (liii set)
) ;import

(check-set-mode! 'report-failed)

;; set-copy
;; 复制一个 set。
;;
;; 语法
;; ----
;; (set-copy set)
;;
;; 参数
;; ----
;; set : set
;; 要复制的 set。
;;
;; 返回值
;; ----
;; set
;; 返回一个新的 set，包含原 set 的所有元素，且比较器相同。
;;
;; 注意
;; ----
;; 返回的是新实例，与原 set 不是同一个对象。
;;
;; 示例
;; ----
;; (set-copy (set 1 2)) => 新的 set，内容与原 set 相同
;;
;; 错误处理
;; ----
;; type-error
;; 如果参数不是 set，抛出异常。

(define s-empty (set))
(define s-1-2 (set 1 2))

(let ((copy (set-copy s-1-2)))
  (check-true (set=? s-1-2 copy))
  (check-false (eq? s-1-2 copy)) ; Ensure new instance
) ;let

(check-true (set-empty? (set-copy s-empty)))
(check-catch 'type-error (set-copy "not a set"))

(check-report)
