(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; copy-time
;; 复制时间对象。
;;
;; 语法
;; ----
;; (copy-time time)
;;
;; 参数
;; ----
;; time : time? 要复制的时间对象。
;;
;; 返回值
;; ----
;; time? 一个新的时间对象，其值与原时间对象相同但独立。
;;
;; 错误处理
;; ----
;; wrong-type-arg 当参数不是时间对象时抛出错误。


;; Test copy-time
(let* ((original (make-time TIME-TAI 777777777 888888888)
       ) ;original
       (copied (copy-time original))
      ) ;
  (check-true (time? copied))
  (check (time-type copied)
    =>
    (time-type original)
  ) ;check
  (check (time-nanosecond copied)
    =>
    (time-nanosecond original)
  ) ;check
  (check (time-second copied)
    =>
    (time-second original)
  ) ;check
  ;; Ensure it's a copy, not the same object
  (check-false (eq? original copied))
  ;; Modify original and ensure copy is unchanged
  (set-time-nanosecond! original
    999999999
  ) ;set-time-nanosecond!
  (check (time-nanosecond copied)
    =>
    777777777
  ) ;check
) ;let*


;; Test error conditions
(check-catch 'wrong-type-arg
  (copy-time "not-a-time")
) ;check-catch


(check-report)
