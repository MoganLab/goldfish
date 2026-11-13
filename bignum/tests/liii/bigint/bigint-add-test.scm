;; bigint-add
;; 计算两个 bigint 的和。
;;
;; 语法
;; ----
;; (bigint-add a b)
;;
;; 参数
;; ----
;; a : bigint?
;;   第一个加数。
;; b : bigint?
;;   第二个加数。
;;
;; 返回值
;; ----
;; bigint?
;;   a 与 b 的和。
;;
;; 错误处理
;; ----
;; type-error
;;   当任一参数不是 bigint 时抛出。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define (b str)
  (string->bigint str)
) ;define

(check (bigint->string (bigint-add (b "1") (b "2"))) => "3")
(check (bigint->string (bigint-add (b "-5") (b "2"))) => "-3")
(check (bigint->string (bigint-add (b "5") (b "-5"))) => "0")
(check (bigint->string (bigint-add (b "4294967296") (b "1"))) => "4294967297")
(check (bigint->string (bigint-add (b "999999999999999999999999999999")
                                   (b "1")))
       => "1000000000000000000000000000000"
) ;check

(check-catch 'type-error (bigint-add 1 (b "2")))
(check-catch 'type-error (bigint-add (b "1") 2))

(check-report)
