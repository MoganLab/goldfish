(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; call-with-current-continuation
;; 捕获当前续延，允许过程在之后恢复执行。
;;
;; 语法
;; ----
;; (call-with-current-continuation proc)
;; (call/cc proc)
;;
;; 参数
;; ----
;; proc : procedure?
;; 接收一个续延对象作为参数的过程。
;;
;; 返回值
;; ------
;; 任意类型
;; proc 的返回值，或续延被调用时传入的值。
;;
;; 说明
;; ----
;; 1. 续延可保存并稍后调用
;; 2. 调用续延会重置调用栈
;; 3. call/cc 是 call-with-current-continuation 的简写
;; 4. 续延可以作为第一类值传递和存储
;; 5. 调用续延时传入的值成为 call/cc 的返回值
;;
;; 示例
;; ----
;; 基本用法：过程正常返回
(check (call/cc (lambda (k) 5)) => 5)

;; 调用续延立即返回
(check (call/cc (lambda (k) (k 10) 20)) => 10)

;; 使用 call-with-current-continuation 完整名称
(check (call-with-current-continuation (lambda (k) (k 'done))) => 'done)

;; 保存续延供后续调用
(let ((saved #f) (calls '()))
  (set! calls (cons 'before calls))
  (let ((result (call/cc (lambda (k) (set! saved k) 'first))))
    (set! calls (cons result calls))
    (when (eq? result 'first)
      (saved 'second)
    ) ;when
  ) ;let
  (check (reverse calls) => '(before first second))
) ;let

;; 从续延返回多个值
(check (call-with-values (lambda () (call/cc (lambda (k) (k 1 2 3))))
         (lambda (a b c) (list a b c))
       ) ;call-with-values
  =>
  '(1 2 3)
) ;check

;; 在嵌套表达式中使用
(check (+ 1 (call/cc (lambda (k) (* 2 (k 3))))) => 4)

;; 实现非局部退出（类似 break）
(check (let ((sum 0))
         (call/cc (lambda (return)
                    (for-each (lambda (x) (when (> x 5) (return sum)) (set! sum (+ sum x)))
                      '(1 2 3 4 5 6 7)
                    ) ;for-each
                    sum
                  ) ;lambda
         ) ;call/cc
       ) ;let
  =>
  15
) ;check

;; 续延只影响调用栈，不恢复变量绑定
(let ((counter 0) (saved #f))
  (call/cc (lambda (k) (set! saved k)))
  (set! counter (+ counter 1))
  (when (< counter 3)
    (saved 'retry)
  ) ;when
  (check counter => 3)
) ;let

;; 用 call/cc 实现简单的生成器（yield 模式）
;; 每次调用生成器返回下一个值
(let ((make-generator (lambda (proc)
                        (let ((return #f) (resume #f))
                          (lambda ()
                            (call/cc (lambda (k)
                                       (set! return k)
                                       (if resume
                                         (resume 'ok)
                                         (begin
                                           (proc (lambda (v) (call/cc (lambda (r) (set! resume r) (return v)))))
                                           (return 'done)
                                         ) ;begin
                                       ) ;if
                                     ) ;lambda
                            ) ;call/cc
                          ) ;lambda
                        ) ;let
                      ) ;lambda
      ) ;make-generator
     ) ;
  (let ((g (make-generator (lambda (yield) (yield 'a) (yield 'b) (yield 'c)))))
    (check (g) => 'a)
    (check (g) => 'b)
    (check (g) => 'c)
    (check (g) => 'done)
  ) ;let
) ;let

;; 用 call/cc 实现异常风格的抛出/捕获
;; throw 是一个接收续延的过程，当调用 throw 时，跳转到 catch 的上下文
(let ((catch-proc (lambda (thunk handler)
                    (call/cc (lambda (k) (let ((result (thunk k))) result)))
                  ) ;lambda
      ) ;catch-proc
      (throw-proc (lambda (k value) (k (list 'exception value))))
     ) ;
  (check (catch-proc (lambda (throw-k) (+ 1 (throw-proc throw-k "error")))
           (lambda (e) e)
         ) ;catch-proc
    =>
    '(exception "error")
  ) ;check
  (check (catch-proc (lambda (throw-k) (+ 1 2)) (lambda (e) e)) => 3)
) ;let

;; 多次调用同一个续延，每次回到同一个"地点"
(let ((checkpoint #f) (results '()))
  (let ((value (call/cc (lambda (k) (set! checkpoint k) 0))))
    (set! results (cons value results))
    (when (< value 3)
      (checkpoint (+ value 1))
    ) ;when
  ) ;let
  (check (reverse results) => '(0 1 2 3))
) ;let

;; 续延作为参数传递给其他函数

(define (escape-if-odd n return)
  (when (odd? n)
    (return 'odd)
  ) ;when
) ;define

(check (let ((n 4)) (call/cc (lambda (k) (escape-if-odd n k) 'even))) => 'even)

(check (let ((n 5)) (call/cc (lambda (k) (escape-if-odd n k) 'even))) => 'odd)

;; 续延对象是一个过程
(check (procedure? (call/cc (lambda (k) k))) => #t)

;; 调用续延后，续延点之后的代码不会执行
(let ((log '()))
  (let ((result (call/cc (lambda (k)
                           (set! log (cons 'before log))
                           (k 'interrupted)
                           (set! log (cons 'after log))
                           'normal
                         ) ;lambda
                ) ;call/cc
        ) ;result
       ) ;
    (set! log (cons 'after-call log))
    (check (reverse log) => '(before after-call))
    (check result => 'interrupted)
  ) ;let
) ;let

(check-report)
