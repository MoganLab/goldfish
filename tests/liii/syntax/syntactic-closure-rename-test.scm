(import (liii check) (liii syntax))

(check-set-mode! 'report-failed)

;; syntactic-closure-rename
;; 取出句法闭包的重命名过程。
;;
;; 语法
;; ----
;; (syntactic-closure-rename sc)
;;
;; 参数
;; ----
;; sc : syntactic-closure
;; 句法闭包对象。
;;
;; 返回值
;; ------
;; procedure or #f
;; 闭包当前的重命名过程；新建闭包初始为 #f，
;; 可通过 syntactic-closure-set-rename! 设置。
;;
;; 错误处理
;; --------
;; 参数不是句法闭包时抛出 type-error。

;; 1. 新建闭包的 rename 槽为 #f
(check (syntactic-closure-rename (make-syntactic-closure (curlet) '() 'x)) => #f)

;; 2. set-rename! 后可读出
(let ((sc (make-syntactic-closure (curlet) '() 'x)))
  (syntactic-closure-set-rename! sc (lambda (i) i))
  (check (procedure? (syntactic-closure-rename sc)) => #t)
  (check ((syntactic-closure-rename sc) 'a) => 'a)
) ;let

;; 错误：非句法闭包参数
(check-catch 'type-error (syntactic-closure-rename 'x))
(check-catch 'type-error (syntactic-closure-rename 42))

(check-report)
