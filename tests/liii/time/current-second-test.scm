(import (liii check)
  (liii time)
  (scheme time)
) ;import


(check-set-mode! 'report-failed)


;; current-second
;; 获取当前时间，以秒为单位。
;;
;; 语法
;; ----
;; (current-second)
;;
;; 参数
;; ----
;; 无参数
;;
;; 返回值
;; ----
;; number?
;; 返回从某个固定时间点（通常是纪元时间）到当前时间的秒数，返回值为浮点数。
;;
;; 注意
;; ----
;; 1. 返回的时间戳通常基于Unix纪元时间（1970-01-01 00:00:00 UTC）
;; 2. 返回值是浮点数类型，支持小数秒精度
;; 3. 精度取决于系统实现，通常为秒级精度
;; 4. 主要用于时间测量和时间戳生成
;;
;; 示例
;; ----
;; (let ((start (current-second)))
;;   (sleep 1)
;;   (let ((end (current-second)))
;;     (display (format "耗时: ~a 秒" (- end start)))))
;;
;; 错误处理
;; ----
;; 无


(let ((t1 (current-second)))
  (check (number? t1) => #t)
  (check (>= t1 0) => #t)
) ;let


(check-report)
