;; dynamic-wind
;; 在过程执行前后执行清理和初始化操作。
;;
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
;; ----
;; 任意类型
;; thunk 的返回值。
;;
;; 说明
;; ----
;; 1. before 和 after 总是成对执行
;; 2. 即使 thunk 抛出异常，after 也会执行
;; 3. 常用于资源管理
;; 4. 与 call/cc 协同：重新进入由 dynamic-wind 保护的动态范围时，
;;    会再次执行同一个 before 过程，离开该范围时再次执行同一个 after 过程。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(let ((log '()))
  (dynamic-wind (lambda () (set! log (cons 'before log)))
    (lambda () (set! log (cons 'thunk log)) 'result)
    (lambda () (set! log (cons 'after log)))
  ) ;dynamic-wind
  (check (reverse log) => '(before thunk after))
) ;let
(let ((log '()))
  (check
    (catch #t
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

;; 重新进入由 dynamic-wind 保护的动态范围
(let ((log '()) (k #f) (first-time #t))
  (dynamic-wind (lambda () (set! log (cons 'before log)))
    (lambda ()
      (set! log (cons 'body log))
      (call/cc (lambda (c) (set! k c)))
      (set! log (cons 'again log))
    ) ;lambda
    (lambda () (set! log (cons 'after log)))
  ) ;dynamic-wind
  (when first-time
    (set! first-time #f)
    (k #f)
  ) ;when
  (check (reverse log) => '(before body again after before again after))
) ;let

;; 嵌套 dynamic-wind 的外层 after 过程恢复自身捕获的值 (#1514)
(let ()
  (define state 'init)
  (define (with-state value thunk)
    (let ((saved state))
      (dynamic-wind (lambda () (set! state value))
        thunk
        (lambda () (set! state saved))
      ) ;dynamic-wind
    ) ;let
  ) ;define

  ;; 第一次调用：单层 dynamic-wind，正常执行后触发优化
  (with-state 'a (lambda () state))
  (check state => 'init)

  ;; 第二次调用：嵌套 dynamic-wind，外层 after 必须恢复自身捕获的值
  (with-state 'outer (lambda () (with-state 'inner (lambda () state))))
  (check state => 'init)
) ;let

(let ()
  (define state 'init)
  (define log '())
  (define (with-state value thunk)
    (let ((saved state))
      (dynamic-wind (lambda () (set! state value))
        thunk
        (lambda () (set! log (cons saved log)) (set! state saved))
      ) ;dynamic-wind
    ) ;let
  ) ;define

  (with-state 'a (lambda () state))
  (with-state 'outer (lambda () (with-state 'inner (lambda () state))))
  (check (reverse log) => '(init outer init))
  (check state => 'init)
) ;let

(check-report)
