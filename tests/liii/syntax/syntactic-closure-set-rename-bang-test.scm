(import (liii check) (liii syntax))

(check-set-mode! 'report-failed)

;; syntactic-closure-set-rename!
;; 设置句法闭包的重命名过程。
;;
;; 语法
;; ----
;; (syntactic-closure-set-rename! sc proc)
;;
;; 参数
;; ----
;; sc : syntactic-closure
;; 句法闭包对象。
;;
;; proc : procedure
;; 重命名过程，接收一个标识符并返回重命名后的标识符。
;;
;; 返回值
;; ------
;; syntactic-closure
;; 返回 sc 本身（已被原地修改）。
;;
;; 说明
;; ----
;; 这是一个破坏性操作。宏展开器在生成模板标识符的重命名闭包时，
;; 通过该过程记录「宏定义环境 → 调用点」的重命名函数。
;;
;; 错误处理
;; --------
;; 第一个参数不是句法闭包时抛出 type-error。

;; 1. 返回闭包本身
(let ((sc (make-syntactic-closure (curlet) '() 'x)))
  (check (eq? (syntactic-closure-set-rename! sc (lambda (i) i)) sc) => #t)
) ;let

;; 2. 设置后 rename 槽生效
(let ((sc (make-syntactic-closure (curlet) '() 'x)))
  (syntactic-closure-set-rename! sc (lambda (i) (string->symbol (string-append "r_" (symbol->string i)))))
  (check ((syntactic-closure-rename sc) 'foo) => 'r_foo)
) ;let

;; 错误：非句法闭包参数
(check-catch 'type-error (syntactic-closure-set-rename! 'x (lambda (i) i)))
(check-catch 'type-error (syntactic-closure-set-rename! 42 (lambda (i) i)))

(check-report)
