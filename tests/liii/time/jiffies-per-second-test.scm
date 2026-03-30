(import (liii check)
        (liii time)
) ;import

(check-set-mode! 'report-failed)

;; jiffies-per-second
;; 获取每秒钟的 jiffy 数量。
;;
;; 语法
;; ----
;; (jiffies-per-second)
;;
;; 参数
;; ----
;; 无参数
;;
;; 返回值
;; ----
;; integer?
;; 返回每秒钟包含的 jiffy 数量，固定值为 1000000。
;;
;; 注意
;; ----
;; 1. 定义 jiffy 与秒之间的换算关系：1 秒 = 1000000 jiffy
;; 2. 返回值是固定的整数，用于时间单位转换
;; 3. 主要用于将 jiffy 时间间隔转换为秒数
;; 4. 与 current-jiffy 配合使用，可以计算精确的时间间隔
;;
;; 示例
;; ----
;; (let ((start (current-jiffy)))
;;   (do-some-work)
;;   (let ((end (current-jiffy)))
;;     (display (format "耗时: ~a 秒"
;;       (/ (- end start) (jiffies-per-second))))))
;;
;; 错误处理
;; ----
;; 无

(check (jiffies-per-second) => 1000000)
(check (integer? (jiffies-per-second)) => #t)
(check (positive? (jiffies-per-second)) => #t)

(check-report)
