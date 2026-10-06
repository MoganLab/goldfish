(import (liii check) (liii syntax))

(check-set-mode! 'report-failed)

;; identifier->symbol
;; 将标识符还原为底层符号。
;;
;; 语法
;; ----
;; (identifier->symbol x)
;;
;; 参数
;; ----
;; x : identifier
;; 符号，或包装了符号的句法闭包。
;;
;; 返回值
;; ------
;; symbol
;; 标识符对应的裸符号；对句法闭包会逐层剥开包装取最内层符号。
;;
;; 错误处理
;; --------
;; x 不是标识符（剥开后不是符号）时抛出 type-error。

;; 1. 裸符号原样返回
(check (identifier->symbol 'x) => 'x)
(check (identifier->symbol 'lambda) => 'lambda)

;; 2. 剥开单层句法闭包
(check (identifier->symbol (make-syntactic-closure (curlet) '() 'x)) => 'x)

;; 3. 剥开多层嵌套句法闭包
(let* ((inner (make-syntactic-closure (curlet) '() 'y))
       (outer (make-syntactic-closure (curlet) '() inner)))
  (check (identifier->symbol outer) => 'y)
) ;let*

;; 错误：非标识符参数
(check-catch 'type-error (identifier->symbol 42))
(check-catch 'type-error (identifier->symbol '(x)))
(check-catch 'type-error (identifier->symbol (make-syntactic-closure (curlet) '() '(+ 1 2))))

(check-report)
