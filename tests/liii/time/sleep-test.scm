(import (liii check)
  (liii time)
  (scheme time)
) ;import


(check-set-mode! 'report-failed)


;; sleep
;; 使当前线程暂停执行指定的秒数。
;;
;; 语法
;; ----
;; (sleep seconds)
;;
;; 参数
;; ----
;; seconds : number?
;; 暂停的时间长度，以秒为单位。可以是整数或浮点数，表示精确的时间间隔。
;;
;; 返回值
;; ----
;; #<unspecified>
;; 返回未指定的值，主要用于其副作用（暂停执行）。
;;
;; 注意
;; ----
;; 1. 暂停当前线程的执行，让出CPU时间给其他线程或进程
;; 2. 实际暂停时间可能受到系统调度精度的影响
;; 3. 对于短时间间隔（如毫秒级），实际暂停时间可能比指定的稍长
;; 4. 参数必须是数值类型，否则会抛出类型错误
;;
;; 错误处理
;; ----
;; type-error 当参数不是数值类型时抛出错误。


;; Test sleep function
(let ((t1 (current-second)))
  (sleep 1)
  (let ((t2 (current-second)))
    (check (>= (ceiling (- t2 t1)) 1) => #t)
  ) ;let
) ;let


(let ((t1 (current-second)))
  (sleep 0.5)
  (let ((t2 (current-second)))
    (check (>= (ceiling (- t2 t1)) 0) => #t)
  ) ;let
) ;let


(check-catch 'type-error
  (sleep 'not-a-number)
) ;check-catch


(check-report)
