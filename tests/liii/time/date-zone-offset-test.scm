(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; date-zone-offset
;; 获取日期对象的时区偏移。
;;
;; 语法
;; ----
;; (date-zone-offset date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 时区偏移（秒）。
;;
;; 示例
;; ----
;; (date-zone-offset (make-date 0 0 0 0 1 1 2023 28800)) => 28800
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
  (check (date-zone-offset d1) => 28800)
  (check (date-zone-offset d2) => -14400)
) ;let


(check-catch 'wrong-type-arg
  (date-zone-offset #f)
) ;check-catch


(check-report)
