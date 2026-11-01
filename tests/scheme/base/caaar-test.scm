;; caaar
;; caaar 是 Scheme 内置函数，等价于 (car (car (car pair)))。
;;
;; 语法
;; ----
;; (caaar pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 三层嵌套的序对或列表，如 (((a b) c) d)。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回 (car (car (car pair)))。
;;
;; 错误处理
;; --------
;; type-error
;; 参数不是序对，或者 car 链上的某一层不是序对时抛出错误。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(check
  (caaar '(((1 2))))
  =>
  1
) ;check
(check
  (caaar '(((a b) c) d))
  =>
  'a
) ;check
(check
  (caaar (cons (cons (cons 1 2) 3) 4))
  =>
  1
) ;check
(check
  (caaar '(((() 5)) 6))
  =>
  '()
) ;check
(check-catch 'type-error (caaar '((1 2))))
(check-catch 'type-error (caaar '(1 (2 3))))
(check-catch 'type-error (caaar 'a))
(check-catch 'type-error (caaar 123))
(check-catch 'type-error (caaar '()))
(check-report)
