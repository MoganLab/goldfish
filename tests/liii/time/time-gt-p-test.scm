(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; time>?
;; 比较第一个时间对象是否大于第二个。
;;
;; 语法
;; ----
;; (time>? time1 time2)
;;
;; 参数
;; ----
;; time1 : time?
;; time2 : time?
;; 两个时间对象，时间类型必须相同。
;;
;; 返回值
;; ----
;; boolean? 如果 time1 > time2 返回 #t，否则返回 #f。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是时间对象或时间类型不匹配时抛出错误。

(let* ((t1 (make-time TIME-UTC 100 5))
       (t3 (make-time TIME-UTC 200 5))
       (t4 (make-time TIME-UTC 0 6)))
  (check (time>? t4 t3) => #t)
  (check (time>? t1 t3) => #f)
) ;let*

;; Test error conditions
(check-catch 'wrong-type-arg
  (time>? (make-time TIME-UTC 0 0) (make-time TIME-TAI 0 0))
) ;check-catch

(check-report)
