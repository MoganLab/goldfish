(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; make-time
;; 创建时间对象。
;;
;; 语法
;; ----
;; (make-time type nanosecond second)
;;
;; 参数
;; ----
;; type : symbol?
;; 时间类型，必须是时间类型常量之一。
;;
;; nanosecond : integer?
;; 纳秒部分，必须在 0-999999999 范围内（包含边界）。
;;
;; second : integer?
;; 秒部分，可以是任意整数。
;;
;; 返回值
;; ----
;; time?
;; 一个新的时间对象。
;;
;; 示例
;; ----
;; (make-time TIME-UTC 0 0)
;; (make-time TIME-MONOTONIC 500000000 1234567890)
;; (make-time TIME-TAI 999999999 -1234567890)
;;
;; 错误处理
;; ----
;; wrong-type-arg 当参数类型不正确时抛出错误。
;; value-error 当类型不是有效的时间类型时抛出错误。


;; Test make-time
(check-true (time? (make-time TIME-UTC 0 0))
) ;check-true
(check-true (time? (make-time TIME-MONOTONIC
                     500000000
                     1234567890
                   ) ;make-time
            ) ;time?
) ;check-true
(check-true (time? (make-time TIME-TAI
                     999999999
                     -1234567890
                   ) ;make-time
            ) ;time?
) ;check-true


;; Test error conditions
(check-catch 'value-error
  (make-time 'invalid-type 0 0)
) ;check-catch
(check-catch 'wrong-type-arg
  (make-time TIME-UTC 'not-number 0)
) ;check-catch
(check-catch 'wrong-type-arg
  (make-time TIME-UTC 0 'not-number)
) ;check-catch


(check-report)
