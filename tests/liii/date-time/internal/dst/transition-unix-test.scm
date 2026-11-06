;; transition-unix
;; 计算某年 POSIX 转移发生的 UNIX 秒时刻。
;;
;; 语法
;; ----
;; (transition-unix offset year transition)
;;
;; 参数
;; ----
;; offset : time-delta? 转移前的本地偏移。
;; year : integer? 年份。
;; transition : tz-transition? 转移定义。
;;
;; 返回值
;; ----
;; integer? 转移发生的 UNIX 秒。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal tz-types)
  (liii date-time internal dst)
) ;import

(check-set-mode! 'report-failed)

(define trans
  (tz-transition (tz-julian-without-leap-day 60) (tz-offset #f 2 0 0))
) ;define

(check (transition-unix (hours-delta 0) 2021 trans) => 1614564000)
(check (transition-unix (hours-delta 1) 2021 trans) => 1614560400)

(check-report)
