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
;; 如果 body 抛出异常，返回匹配子句的结果；否则返回 else 子句的结果。
;;
;; 说明
;; ----
;; 1. 捕获 body 中抛出的异常
;; 2. 使用类似 cond 的子句匹配异常
;; 3. else 子句作为默认处理
;; 4. 注意：S7 的 guard 即使 body 未抛出异常也会返回 else 子句的结果
(check (guard (ex (else 'caught))
         (raise 'error)
       ) ;guard
  =>
  'caught
) ;check
(check (guard (ex ((eq? ex 'specific) 'matched)
                (else 'default)
              ) ;ex
         (raise 'specific)
       ) ;guard
  =>
  'matched
) ;check
(check (guard (ex (else 'caught))
         'normal
       ) ;guard
  =>
  'caught
) ;check
(check (guard (ex ((eq? ex 'a) 'a-caught)
               ((eq? ex 'b) 'b-caught)
              ) ;ex
         (raise 'b)
       ) ;guard
  =>
  'b-caught
) ;check
(check (guard (ex (else ex))
         (error 'test-error "message")
       ) ;guard
  =>
  "message"
) ;check

(check-report)
