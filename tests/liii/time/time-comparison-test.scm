(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; time<=? time<? time=? time>=? time>?
;; 比较两个时间对象的大小。
;;
;; 语法
;; ----
;; (time<=? time1 time2)
;; (time<?  time1 time2)
;; (time=?  time1 time2)
;; (time>=? time1 time2)
;; (time>?  time1 time2)
;;
;; 参数
;; ----
;; time1 : time?
;; time2 : time?
;; 两个时间对象，时间类型必须相同。
;;
;; 返回值
;; ----
;; boolean? 返回比较结果。
;;
;; 错误处理
;; ----
;; wrong-type-arg 当参数不是时间对象或时间类型不匹配时抛出错误。

;; Test time comparison
(let* ((t1 (make-time TIME-UTC 100 5))
       (t2 (make-time TIME-UTC 100 5))
       (t3 (make-time TIME-UTC 200 5))
       (t4 (make-time TIME-UTC 0 6)))
  (check (time=? t1 t2) => #t)
  (check (time<? t1 t3) => #t)
  (check (time<=? t1 t3) => #t)
  (check (time>? t4 t3) => #t)
  (check (time>=? t4 t3) => #t)
  (check (time<? t3 t1) => #f)
  (check (time>? t1 t4) => #f)
) ;let*

;; Test comparison error conditions
(check-catch 'wrong-type-arg
  (time<? (make-time TIME-UTC 0 0)
          (make-time TIME-TAI 0 0)
  ) ;time<?
) ;check-catch
(check-catch 'wrong-type-arg
  (time=? "not-time" (make-time TIME-UTC 0 0))
) ;check-catch

(check-report)
