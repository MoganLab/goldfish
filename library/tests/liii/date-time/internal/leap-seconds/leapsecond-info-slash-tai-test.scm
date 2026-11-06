;; leapsecond-info/tai
;; 按 TAI 秒查询闰秒信息，返回是否闰秒与当前 TAI 偏移。
;;
;; 语法
;; ----
;; (leapsecond-info/tai posix-tai)
;;
;; 参数
;; ----
;; posix-tai : integer? 以 TAI 计的纪元秒数。
;;
;; 返回值
;; ----
;; values boolean? 是否恰好处于闰秒插入，与 integer? 累计 TAI 偏移秒数。

(import (liii check) (liii date-time internal leap-seconds))

(check-set-mode! 'report-failed)

(check (call-with-values (lambda () (leapsecond-info/tai 0)) list) => '(#f 10))
(check (call-with-values (lambda () (leapsecond-info/tai 63072010)) list)
  =>
  '(#f 10)
) ;check
(check (call-with-values (lambda () (leapsecond-info/tai 78796811)) list)
  =>
  '(#f 11)
) ;check

(check-report)
