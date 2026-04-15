(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; date->julian-day
;; 将日期对象转换为儒略日。
;;
;; 语法
;; ----
;; (date->julian-day date)
;;
;; 参数
;; ----
;; date : date? 要转换的日期对象。
;;
;; 返回值
;; ----
;; number? 儒略日数值。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是日期对象时抛出错误。


;; date->julian-day basic (UTC epoch)
(let ((d (make-date 0 0 0 0 1 1 1970 0)))
  (check (date->julian-day d)
    =>
    4881175/2
  ) ;check

  ;; With different dates
  (let ((d1 (make-date 0 0 0 0 1 1 2000 0)))
    (check (> (date->julian-day d1)
             (date->julian-day d)
           ) ;>
      =>
      #t
    ) ;check
  ) ;let
) ;let


(check-report)
