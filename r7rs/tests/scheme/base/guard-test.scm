;; guard
;; 捕获异常并进行条件处理。
;;
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
;; ----
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

(import (liii check))
(import (scheme base))
(import (scheme read))

(check-set-mode! 'report-failed)

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

;; 3.1 body 正常返回 pair 等单值时原样返回（不与异常标记混淆）
(check (guard (ex (else 'err)) (cons 1 2)) => '(1 . 2))

;; 3.2 body 返回 (values) 时透传（S7 中 (values) 产生单个未指定值）
(check
  (call-with-values
    (lambda () (guard (ex (else "err")) (values)))
    (lambda xs (length xs))
  ) ;call-with-values
  =>
  1
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

;; 7. 卫生性（Issue 1341）：条件变量命名为内部使用的标识符时宏不被破坏
(check
  (guard (cond (#t cond))
    (error 'x "boom")
    'no-error
  ) ;guard
  =>
  "boom"
) ;check

(check (guard (else (#t else)) (error 'x "e1")) => "e1")
(check (guard (apply (#t apply)) (error 'x "e2")) => "e2")
(check (guard (throw (#t throw)) (error 'x "e3")) => "e3")
(check (guard (values (#t values)) (error 'x "e4")) => "e4")
(check (guard (catch (#t catch)) (error 'x "e5")) => "e5")

;; 8. 卫生性：使用点局部遮蔽 cond/apply/throw/values 等标识符不影响 guard
(check
  (let ((cond 42) (apply 43) (throw 44) (values 45) (catch 46))
    (guard (e (#t e))
      (error 'x "shadowed")
    ) ;guard
  ) ;let
  =>
  "shadowed"
) ;check

;; 8.1 遮蔽环境下正常返回（遮蔽 values 时用户自身的 values 调用也会被遮蔽，
;; 故多值场景不遮蔽 values）
(check
  (let ((cond 42) (apply 43) (throw 44))
    (guard (e (else 'err)) 'normal)
  ) ;let
  =>
  'normal
) ;check

(check
  (call-with-values
    (lambda ()
      (let ((cond 42) (apply 43) (throw 44))
        (guard (e (else 'err)) (values 1 2))
      ) ;let
    ) ;lambda
    (lambda (a b) (+ (* 10 a) b))
  ) ;call-with-values
  =>
  12
) ;check

;; 8.2 遮蔽环境下 reraise 交由外层 guard 捕获
(check
  (let ((cond 42) (apply 43) (throw 44))
    (guard (outer (#t outer))
      (guard (e ((eq? e 1) "one"))
        (raise 2)
      ) ;guard
    ) ;guard
  ) ;let
  =>
  2
) ;check

;; 8.3 遮蔽环境下 => 子句
(check
  (let ((cond 42) (apply 43))
    (guard (ex ((and (integer? ex) (* ex 100)) => (lambda (r) (+ ex r))))
      (raise 5)
    ) ;guard
  ) ;let
  =>
  505
) ;check

;; 9. guard 与 error-object 深度联动测试

;; 9.1 guard 模式匹配谓词分发 (file-error? / read-error? / error-object?)
(check
  (guard (ex
          ((file-error? ex) 'handled-file)
          ((read-error? ex) 'handled-read)
          ((error-object? ex) (error-object-message ex))
          (else 'unknown))
    (open-input-file "tests/scheme/base/nonexistent-file-12345.scm"))
  =>
  'handled-file
) ;check

(check
  (guard (ex
          ((file-error? ex) 'handled-file)
          ((read-error? ex) 'handled-read)
          ((error-object? ex) (error-object-message ex))
          (else 'unknown))
    (read (open-input-string "(")))
  =>
  'handled-read
) ;check

(check
  (guard (ex
          ((file-error? ex) 'handled-file)
          ((error-object? ex) (list (error-object-message ex) (error-object-irritants ex)))
          (else 'unknown))
    (error "standard failure" 100 200))
  =>
  '("standard failure" (100 200))
) ;check

;; 9.2 guard => 接收器子句处理 error-object
(check
  (guard (ex
          ((and (error-object? ex) ex) => (lambda (err) (string-append "prefix: " (error-object-message err))))
          (else "fallback"))
    (error "core dumped"))
  =>
  "prefix: core dumped"
) ;check

;; 9.3 嵌套 guard 未匹配 re-raise 透传保持 error-object 完整性
(check
  (guard (outer
          ((file-error? outer) (list 'outer-caught (error-object-message outer)))
          (else 'unexpected))
    (guard (inner
            ((read-error? inner) 'handled-read))
      ;; 抛出 file-error，内层只处理 read-error，自动在原动态环境 re-raise 给外层
      (error 'file-error "no such file: data.db")))
  =>
  '(outer-caught "no such file: data.db")
) ;check

;; 9.4 遮蔽内部关键字环境下 guard 与 error-object 正常联动
(check
  (let ((cond 'shadowed) (apply 'shadowed) (throw 'shadowed))
    (guard (ex
            ((error-object? ex) (error-object-message ex))
            (else 'failed))
      (error "hygiene ok")))
  =>
  "hygiene ok"
) ;check

(check-report)
