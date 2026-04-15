(import (liii check) (liii time))


(check-set-mode! 'report-failed)


;; current-jiffy
;; 获取当前时间的 jiffy 计数。
;;
;; 语法
;; ----
;; (current-jiffy)
;;
;; 参数
;; ----
;; 无参数
;;
;; 返回值
;; ----
;; integer?
;; 返回从某个固定时间点（通常是纪元时间）到当前时间的 jiffy 计数，返回值为整数。
;;
;; 注意
;; ----
;; 1. jiffy 是时间测量单位，1 jiffy = 1/1,000,000 秒（微秒）
;; 2. 返回值是整数类型，提供比秒更精确的时间测量
;; 3. 主要用于高精度时间测量和性能分析
;; 4. 与 current-second 的关系：current-jiffy = (round (* current-second 1000000))
;;
;; 示例
;; ----
;; (let ((start (current-jiffy)))
;;   (do-some-work)
;;   (let ((end (current-jiffy)))
;;     (display (format "耗时: ~a 微秒" (- end start)))))
;;
;; 错误处理
;; ----
;; 无


(let ((j1 (current-jiffy)))
  (check (integer? j1) => #t)
  (check (>= j1 0) => #t)
) ;let


(check-report)
