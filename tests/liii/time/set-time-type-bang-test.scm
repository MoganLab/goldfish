(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; set-time-type!
;; 设置时间对象的类型。
;;
;; 语法
;; ----
;; (set-time-type! time type)
;;
;; 参数
;; ----
;; time : time? 要修改的时间对象。
;; type : symbol? 新的时间类型（TIME-UTC, TIME-TAI, TIME-MONOTONIC, TIME-DURATION）。
;;
;; 返回值
;; ----
;; symbol? 返回被设定的新类型。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数类型不正确时抛出错误。


(let ((t (make-time TIME-UTC 0 0)))
  (check (set-time-type! t TIME-MONOTONIC)
    =>
    TIME-MONOTONIC
  ) ;check
  (check (time-type t) => TIME-MONOTONIC)
  (check (set-time-type! t TIME-TAI)
    =>
    TIME-TAI
  ) ;check
  (check (time-type t) => TIME-TAI)
) ;let


;; Test error conditions
(let ((t (make-time TIME-UTC 0 0)))
  (check-catch 'wrong-type-arg
    (set-time-type! "not-a-time"
      TIME-MONOTONIC
    ) ;set-time-type!
  ) ;check-catch
  (check (set-time-type! t 'invalid-type)
    =>
    'invalid-type
  ) ;check
) ;let


(check-report)
