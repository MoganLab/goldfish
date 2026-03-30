(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; date->modified-julian-day
;; 将日期对象转换为修正儒略日。
;;
;; 语法
;; ----
;; (date->modified-julian-day date)
;;
;; 参数
;; ----
;; date : date? 要转换的日期对象。
;;
;; 返回值
;; ----
;; integer? 修正儒略日数值。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是日期对象时抛出错误。

;; date->modified-julian-day basic (UTC epoch)
(let ((d (make-date 0 0 0 0 1 1 1970 0)))
  (check (date->modified-julian-day d) => 40587)

  ;; With different dates
  (let ((d1 (make-date 0 0 0 0 1 1 2000 0)))
    (check (> (date->modified-julian-day d1) (date->modified-julian-day d)) => #t)
  ) ;let
) ;let

(check-report)
