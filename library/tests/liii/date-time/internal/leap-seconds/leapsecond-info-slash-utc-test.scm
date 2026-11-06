;; leapsecond-info/utc
;; 按 UTC 秒查询闰秒信息，返回是否闰秒与当前 TAI 偏移。
;;
;; 语法
;; ----
;; (leapsecond-info/utc posix-utc)
;;
;; 参数
;; ----
;; posix-utc : integer? UNIX 纪元起的 UTC 秒数。
;;
;; 返回值
;; ----
;; values boolean? 该秒是否为闰秒插入点，与 integer? 累计 TAI 偏移秒数。

(import (liii check) (liii date-time internal leap-seconds))

(check-set-mode! 'report-failed)

(check (call-with-values (lambda () (leapsecond-info/utc 63072000)) list)
  =>
  '(#t 10)
) ;check
(check (call-with-values (lambda () (leapsecond-info/utc 0)) list) => '(#f 10))
(check (call-with-values (lambda () (leapsecond-info/utc 2000000000)) list)
  =>
  '(#f 37)
) ;check

(check-report)
