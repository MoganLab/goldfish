;; list%fold-left
;; 左折叠（对标 Scala List.foldLeft / fold）。
;;
;; 语法
;; ----
;; ($ xs :fold-left init f)
;; ($ xs :fold init)
;;
;; 参数
;; ----
;; init : any
;; 初始累加器。
;;
;; f : procedure?
;; 二参函数 (acc elem) => 新累加器，左结合。
;;
;; 返回值
;; ------
;; any
;; 最终累加器；空表返回 init。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :fold-left 0 +) => 6)
(check ($ '(1 2 3) :fold-left 0 -) => -6)
(check ($ '() :fold-left 5 +) => 5)
(check ($ '(1 2 3) :fold 1 *) => 6)
(check ($ '(1 2 3) :fold-left 1 (lambda (acc e) (* acc 2))) => 8)

(check-report)
