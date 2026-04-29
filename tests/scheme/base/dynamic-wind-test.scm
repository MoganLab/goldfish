(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; dynamic-wind
;; 在过程执行前后执行清理和初始化操作。
;;
;; 语法
;; ----
;; (dynamic-wind before thunk after)
;;
;; 参数
;; ----
;; before : procedure?
;; 无参数过程，在 thunk 前执行。
;; thunk : procedure?
;; 无参数过程，主体操作。
;; after : procedure?
;; 无参数过程，在 thunk 后执行（包括异常退出）。
;;
;; 返回值
;; ------
;; 任意类型
;; thunk 的返回值。
;;
;; 说明
;; ----
;; 1. before 和 after 总是成对执行
;; 2. 即使 thunk 抛出异常，after 也会执行
;; 3. 常用于资源管理
(let ((log '()))
  (dynamic-wind (lambda () (set! log (cons 'before log)))
    (lambda () (set! log (cons 'thunk log)) 'result)
    (lambda () (set! log (cons 'after log)))
  ) ;dynamic-wind
  (check (reverse log) => '(before thunk after))
) ;let
(let ((log '()))
  (check (catch #t
           (lambda ()
             (dynamic-wind (lambda () (set! log (cons 'before log)))
               (lambda () (error 'test-error))
               (lambda () (set! log (cons 'after log)))
             ) ;dynamic-wind
           ) ;lambda
           (lambda args 'caught)
         ) ;catch
    =>
    'caught
  ) ;check
  (check (reverse log) => '(before after))
) ;let

(check-report)
