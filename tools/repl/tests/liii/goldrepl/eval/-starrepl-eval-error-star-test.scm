;; *repl-eval-error*
;; 求值失败时使用的哨兵值。
;;
;; 语法
;; ----
;; *repl-eval-error*
;;
;; 参数
;; ----
;; 无
;; 该变量不接受参数。
;;
;; 返回值
;; ----
;; list
;; 形如 `(eval-error)` 的列表，作为求值失败的哨兵值。

(import (scheme base) (liii check) (liii goldrepl eval))

(check-set-mode! 'report-failed)

;; 哨兵值是一个列表
(check (list? *repl-eval-error*) => #t)

;; 哨兵值的首元素是 eval-error 符号
(check (car *repl-eval-error*) => 'eval-error)

(check-report)
