(import (liii check)
        (liii date-time current-time))

(check-set-mode! 'report-failed)

;; current-monotonic-seconds
;; 获取系统自开机以来的单调秒数，返回精确整数。
;;
;; 语法
;; ----
;; (current-monotonic-seconds)
;;
;; 返回值
;; ----
;; 精确整数 (exact? and integer?)
;;
;; 示例
;; ----
;; (current-monotonic-seconds)

(let ((sec (current-monotonic-seconds)))
  (check-true (number? sec))
  (check-true (exact? sec))
  (check-true (integer? sec))
  (check-true (>= sec 0))
  (check-true (< sec 1000000000))
  (let ((sec2 (current-monotonic-seconds)))
    (check-true (exact? sec2))
    (check-true (>= sec2 sec))
  ) ;let
) ;let

(check-report)
