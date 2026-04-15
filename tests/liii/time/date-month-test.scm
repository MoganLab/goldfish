(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; date-month
;; 获取日期对象的月部分。
;;
;; 语法
;; ----
;; (date-month date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 月部分（1-12）。
;;
;; 示例
;; ----
;; (date-month (make-date 0 0 0 0 1 12 2023 0)) => 12
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是日期对象时抛出错误。


(let ((d1 (make-date 123456789
            45
            30
            14
            25
            12
            2023
            28800
          ) ;make-date
      ) ;d1
      (d2 (make-date 999999999
            59
            59
            23
            31
            1
            2000
            -14400
          ) ;make-date
      ) ;d2
     ) ;
  (check (date-month d1) => 12)
  (check (date-month d2) => 1)
) ;let


(check-catch 'wrong-type-arg
  (date-month (cons 1 2))
) ;check-catch


(check-report)
