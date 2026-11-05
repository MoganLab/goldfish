;; *repl-value-count*
;; 已经注册的会话值数量。
;;
;; 语法
;; ----
;; *repl-value-count*
;;
;; 参数
;; ----
;; 无
;; 该变量不接受参数。
;;
;; 返回值
;; ----
;; integer
;; 已经注册的会话值数量。

(import (scheme base) (liii check) (liii string) (liii goldrepl eval))

(check-set-mode! 'report-failed)

;; 计数器是一个非负整数
(check (number? *repl-value-count*) => #t)
(check (integer? *repl-value-count*) => #t)
(check (>= *repl-value-count* 0) => #t)

;; 从注册得到的变量名中解析出计数器序号

(define (registered-index sym)
  (string->number (substring (symbol->string sym) 1))
) ;define

;; 每注册一个会话值，计数器序号递增 1
(let* ((sym1 (%repl-register-value! 1)) (sym2 (%repl-register-value! 2)))
  (check (registered-index sym1) => 1)
  (check (registered-index sym2) => 2)
) ;let*

(check-report)
