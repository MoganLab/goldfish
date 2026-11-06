;; make-chain-comparator
;; 构造支持链式比较的比较器工厂。
;;
;; 语法
;; ----
;; (make-chain-comparator pred type-check msg compare-impl)
;;
;; 参数
;; ----
;; pred : procedure? 对 compare-impl 结果判定的谓词。
;; type-check : procedure? 参数类型检查。
;; msg : string? 类型错误消息。
;; compare-impl : procedure? 三值比较实现。
;;
;; 返回值
;; ----
;; procedure? 接受两个及以上参数、返回 boolean? 的链式比较器。

(import (liii check) (liii date-time internal util))

(check-set-mode! 'report-failed)

(define =0
  (make-chain-comparator (lambda (r) (= r 0))
    number?
    "chain: expected a number"
    -
  ) ;make-chain-comparator
) ;define

(check-true (=0 1 1))
(check-false (=0 1 2))
(check-true (=0 1 1 1))
(check-false (=0 1 1 2))
(check-catch 'type-error (=0 1 "a"))

(check-report)
