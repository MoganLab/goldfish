(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)

;; guard
;; 捕获异常并进行条件处理。
;;
;; 语法
;; ----
;; (guard (var clause ...) body ...)
;;
;; 参数
;; ----
;; var : symbol
;; 绑定到异常对象的变量。
;; clause ... : 条件子句
;; 类似 cond 的条件子句。
;; body ... : 表达式体
;; 可能引发异常的表达式。
;;
;; 返回值
;; ------
;; any
;; 如果 body 正常求值，返回 body 的求值结果（支持多值）；
;; 如果 body 抛出异常，返回匹配子句的结果；
;; 若无匹配子句且无 else，则重新抛出（re-raise）该异常。
;;
;; 说明
;; ----
;; 1. body 正常执行无异常时直接返回其值，不会进入 else 或其他条件子句
;; 2. 只有当 body 抛出异常时，才会将异常对象绑定到 var 并按类似 cond 的子句匹配
;; 3. else 子句作为默认异常处理
;; 4. 若所有条件子句均未匹配且无 else 子句，则在 guard 表达式的动态环境中重新抛出异常
;; 5. body 正常返回多值（values）时会被完整透传

;; 1. body 正常求值返回其值（不进入 else 或 cond 子句）
(check (guard (ex (else 'caught)) 'normal) => 'normal)
(check (guard (ex (else "error!")) (+ 1 2)) => 3)
(check
  (guard (ex ((eq? ex 42) "matched 42") (else "default"))
    42
  ) ;guard
  =>
  42
) ;check

;; 2. body 包含多个表达式顺序执行
(check
  (let ((flag #f))
    (guard (ex (else 'err))
      (set! flag #t)
      42
    ) ;guard
  ) ;let
  =>
  42
) ;check

;; 3. body 正常返回多值（values）时完整透传
(check
  (call-with-values
    (lambda () (guard (ex (else "err")) (values 10 20)))
    (lambda (a b) (+ a b))
  ) ;call-with-values
  =>
  30
) ;check

;; 4. 捕获 raise 抛出的异常并触发 else 或 cond 子句
(check (guard (ex (else 'caught)) (raise 'error)) => 'caught)
(check
  (guard (ex ((eq? ex 42) "matched 42") (else "default"))
    (raise 42)
  ) ;guard
  =>
  "matched 42"
) ;check
(check
  (guard (ex ((eq? ex 'specific) 'matched) (else 'default))
    (raise 'specific)
  ) ;guard
  =>
  'matched
) ;check
(check
  (guard (ex ((eq? ex 'a) 'a-caught) ((eq? ex 'b) 'b-caught))
    (raise 'b)
  ) ;guard
  =>
  'b-caught
) ;check
(check (guard (ex (else ex)) (error 'test-error "message")) => "message")

;; 5. cond 支持 => 语法，接收者拿到的是测试表达式的值
(check
  (guard
    (ex ((and (integer? ex) (* ex 100)) => (lambda (r) (+ ex r))))
    (raise 5)
  ) ;guard
  =>
  505
) ;check

;; 6. 无匹配 cond 子句且无 else 时重新抛出异常（re-raise）
(check-catch #t (guard (ex ((eq? ex 1) "got 1")) (raise 2)))
(check
  (guard (outer ((eq? outer 2) "re-raised 2"))
    (guard (ex ((eq? ex 1) "got 1"))
      (raise 2)
    ) ;guard
  ) ;guard
  =>
  "re-raised 2"
) ;check

(check-report)
