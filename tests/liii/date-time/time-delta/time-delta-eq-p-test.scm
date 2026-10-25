(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; time-delta=?
;; 判断两个 time-delta 是否逐分量相等。
;;
;; 语法
;; ----
;; (time-delta=? delta1 delta2)
;;
;; 参数
;; ----
;; delta1, delta2 : time-delta? 时间增量
;;
;; 返回值
;; ----
;; boolean? 七个分量（年/月/周/日/时/分/秒）全部相等时为 #t。
;;
;; 说明
;; ----
;; 逐分量比较，不做进位换算：(hours-delta 8) 与 (minutes-delta 480)
;; 表示的总时长相同，但分量不同，不相等。
;;
;; 示例
;; ----
;; (time-delta=? (hours-delta 8) (hours-delta 8)) => #t

(check-true (time-delta=? (hours-delta 8) (hours-delta 8)))
(check-true (time-delta=? (seconds-delta 0) (seconds-delta 0)))
(check-true (time-delta=? (hours-delta -5) (hours-delta -5)))
(check-true
  (time-delta=?
    (time-delta+ (hours-delta 1) (minutes-delta 30))
    (time-delta+ (hours-delta 1) (minutes-delta 30))))
(check-true
  (time-delta=?
    (make-time-delta 1 2 3 4 5 6 7)
    (make-time-delta 1 2 3 4 5 6 7)))

(check-false (time-delta=? (hours-delta 8) (hours-delta 9)))
(check-false (time-delta=? (hours-delta 8) (hours-delta -8)))
;; 逐分量比较：总时长相同但分量不同
(check-false (time-delta=? (hours-delta 8) (minutes-delta 480)))
(check-false
  (time-delta=?
    (make-time-delta 1 0 0 0 0 0 0)
    (make-time-delta 0 12 0 0 0 0 0)))

;; 非法参数抛出 type-error
(check-catch 'type-error (time-delta=? (hours-delta 8) "not a delta"))
(check-catch 'type-error (time-delta=? 42 (hours-delta 8)))
(check-catch 'type-error (time-delta=? 'sym 'sym))

(check-report)
