(import (liii check) (liii syntax))

(check-set-mode! 'report-failed)

;; syntactic-closure-env
;; 取出句法闭包捕获的词法环境。
;;
;; 语法
;; ----
;; (syntactic-closure-env sc)
;;
;; 参数
;; ----
;; sc : syntactic-closure
;; 句法闭包对象。
;;
;; 返回值
;; ------
;; environment
;; 创建闭包时传入的 env，与传入环境 eq? 相等。
;;
;; 错误处理
;; --------
;; 参数不是句法闭包时抛出 type-error。

(let* ((e (curlet))
       (sc (make-syntactic-closure e '() 'x)))
  (check (eq? (syntactic-closure-env sc) e) => #t)
) ;let*

(let ((sc (make-syntactic-closure (rootlet) '() 'x)))
  (check (eq? (syntactic-closure-env sc) (rootlet)) => #t)
) ;let

;; 错误：非句法闭包参数
(check-catch 'type-error (syntactic-closure-env 'x))
(check-catch 'type-error (syntactic-closure-env 42))
(check-catch 'type-error (syntactic-closure-env '(a b)))

(check-report)
