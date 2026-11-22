;; guard
;; guard 嵌套在 with-exception-handler 内时对 raise/raise-continuable 的拦截与
;; re-raise 语义（R7RS 6.11）。
;;
;;
;; 语法
;; ----
;; (guard (var clause ...) body ...) 嵌套在
;; (with-exception-handler handler thunk) 内使用
;;
;; 参数
;; ----
;; var : symbol
;; 绑定到异常对象的变量。
;; clause ... : 条件子句
;; 类似 cond 的条件子句。
;; handler : procedure?
;; 外层 with-exception-handler 安装的异常处理器。
;;
;; 返回值
;; ----
;; any
;; body 正常求值时返回其值；子句匹配时返回子句值；无匹配子句时把原始条件
;; re-raise 给外层 with-exception-handler。
;;
;; 说明
;; ----
;; 1. R7RS 的 guard 在语义上安装一个异常处理器，必须能拦截 raise/raise-continuable，
;;    而不仅仅是 s7 原生的 throw/catch
;; 2. 当 guard 嵌套在 with-exception-handler 内时，body 中的 raise 应优先被 guard
;;    捕获并按子句分发，外层 handler 不触发
;; 3. 仅当 guard 没有匹配子句（且无 else）时，才把原始条件 re-raise 给外层
;;    with-exception-handler
;; 4. 固化 R7RS 6.11 与 SRFI-34 示例 #5/#6/#7 的预期行为

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; ---- SRFI-34 示例 #5/#6/#7：guard 在 with-exception-handler 内 ----

(define (test-exception-handler-4 v out)
  (call-with-current-continuation
    (lambda (k)
      (with-exception-handler (lambda (x) (display "reraised " out) (write x out) (display #\! out) (k 'zero))
        (lambda ()
          (guard (condition ((positive? condition) 'positive) ((negative? condition) 'negative))
            (raise v)
          ) ;guard
        ) ;lambda
      ) ;with-exception-handler
    ) ;lambda
  ) ;call-with-current-continuation
) ;define

;; #5: v=1，guard 匹配 positive?，返回 'positive，外层 handler 不触发
(let* ((out (open-output-string)) (value (test-exception-handler-4 1 out)))
  (check value => 'positive)
  (check (get-output-string out) => "")
) ;let*

;; #6: v=-1，guard 匹配 negative?，返回 'negative，外层 handler 不触发
(let* ((out (open-output-string)) (value (test-exception-handler-4 -1 out)))
  (check value => 'negative)
  (check (get-output-string out) => "")
) ;let*

;; #7: v=0，guard 无匹配子句，re-raise 原始条件 0 给外层 handler
(let* ((out (open-output-string)) (value (test-exception-handler-4 0 out)))
  (check value => 'zero)
  (check (get-output-string out) => "reraised 0!")
) ;let*

;; ---- guard 捕获 raise 时不应触发外层 with-exception-handler ----
(check
  (with-exception-handler (lambda (x) 'should-not-be-called)
    (lambda ()
      (guard (condition ((positive? condition) 'positive) ((negative? condition) 'negative))
        (raise 1)
      ) ;guard
    ) ;lambda
  ) ;with-exception-handler
  =>
  'positive
) ;check

(check
  (with-exception-handler (lambda (x) 'should-not-be-called)
    (lambda ()
      (guard (condition ((positive? condition) 'positive) ((negative? condition) 'negative))
        (raise -1)
      ) ;guard
    ) ;lambda
  ) ;with-exception-handler
  =>
  'negative
) ;check

;; ---- guard 未匹配时 re-raise 到外层 with-exception-handler ----
;; R7RS：handler 若对非 continuable 异常正常返回（不跳转）属于程序错误，
;; 故此处用 call/cc 跳转取回结果（同 SRFI-34 示例 #5–#7 的写法）。
(check
  (call-with-current-continuation
    (lambda (k)
      (with-exception-handler (lambda (x) (k (list 'outer x)))
        (lambda () (guard (condition ((zero? condition) 'zero)) (raise 7)))
      ) ;with-exception-handler
    ) ;lambda
  ) ;call-with-current-continuation
  =>
  '(outer 7)
) ;check

;; ---- guard 与 raise-continuable 在 with-exception-handler 内 ----
;; R7RS 语义：guard 的隐式 handler 通过 escape 返回子句值，因此 guard 内部的
;; raise-continuable 不会"恢复"到调用点——guard 直接以匹配子句的值逃逸返回。
;; 此处验证 guard 仍能优先拦截 raise-continuable，而非交给外层 fallback。
(check
  (with-exception-handler (lambda (x) 'fallback)
    (lambda ()
      (guard (condition ((number? condition) (* condition 10)))
        (+ 1 (raise-continuable 5))
      ) ;guard
    ) ;lambda
  ) ;with-exception-handler
  =>
  50
) ;check

(check-report)
