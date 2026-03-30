(import (liii check)
        (liii time)
        (srfi srfi-1)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; TIME-DURATION TIME-MONOTONIC TIME-PROCESS TIME-TAI TIME-THREAD TIME-UTC
;; 时间类型常量，用于标识不同类型的时间。
;;
;; 说明
;; ----
;; 这些常量表示不同的时钟类型：
;; - TIME-DURATION : 时间间隔（无起始点）
;; - TIME-MONOTONIC: 单调递增的时钟（不受系统时间调整影响）
;; - TIME-PROCESS  : 进程使用的CPU时间
;; - TIME-TAI      : 国际原子时
;; - TIME-THREAD   : 线程使用的CPU时间
;; - TIME-UTC      : 协调世界时
;;
;; 每个常量对应一个唯一的符号值，用于指定时间对象的类型。
;;
;; 示例
;; ----
;; TIME-UTC => time-utc
;; TIME-TAI => time-tai

;; Test time constants
(check-true (symbol? TIME-DURATION))
(check-true (symbol? TIME-MONOTONIC))
(check-true (symbol? TIME-PROCESS))
(check-true (symbol? TIME-TAI))
(check-true (symbol? TIME-THREAD))
(check-true (symbol? TIME-UTC))

;; Ensure all constants are distinct
(let ((constants (list TIME-DURATION TIME-MONOTONIC TIME-PROCESS
                       TIME-TAI TIME-THREAD TIME-UTC)))
  (check-true (= (length constants) (length (delete-duplicates constants))))
) ;let

(check-report)
