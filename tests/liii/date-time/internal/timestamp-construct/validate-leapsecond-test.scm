;; validate-leapsecond
;; 校验时间戳的秒字段是否落在合法闰秒上，成功调用 ok、失败调用 err。
;;
;; 语法
;; ----
;; (validate-leapsecond ts ok err)
;;
;; 参数
;; ----
;; ts : timestamp? 待校验的时间戳。
;; ok : procedure? 校验通过时调用的 thunk。
;; err : procedure? 校验失败时调用的 thunk。
;;
;; 返回值
;; ----
;; ok 或 err 的返回值。

(import (liii check)
  (liii date-time date)
  (liii date-time clock-time)
  (liii date-time timezone)
  (liii date-time internal timestamp-types)
  (liii date-time internal timestamp-construct)
) ;import

(check-set-mode! 'report-failed)

(define (mk y m d h mi s)
  (%make-timestamp (make-date y m d) (make-clock-time h mi s) (utc-timezone) 0)
) ;define

(check (validate-leapsecond (mk 2021 1 1 0 0 30) (lambda () 'ok) (lambda () 'err))
  =>
  'ok
) ;check
(check (validate-leapsecond (mk 2016 12 31 23 59 60) (lambda () 'ok) (lambda () 'err))
  =>
  'ok
) ;check
(check (validate-leapsecond (mk 2021 12 31 23 59 60) (lambda () 'ok) (lambda () 'err))
  =>
  'err
) ;check

(check-report)
