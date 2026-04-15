(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; date-hour
;; 获取日期对象的小时部分。
;;
;; 语法
;; ----
;; (date-hour date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 小时部分（0-23）。
;;
;; 示例
;; ----
;; (date-hour (make-date 0 0 0 14 1 1 2023 0)) => 14
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
  (check (date-hour d1) => 14)
  (check (date-hour d2) => 23)
) ;let


(check-catch 'wrong-type-arg
  (date-hour #t)
) ;check-catch


(check-report)
