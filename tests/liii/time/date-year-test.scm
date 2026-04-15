(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; date-year
;; 获取日期对象的年部分。
;;
;; 语法
;; ----
;; (date-year date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 年部分。
;;
;; 示例
;; ----
;; (date-year (make-date 0 0 0 0 1 1 2023 0)) => 2023
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
  (check (date-year d1) => 2023)
  (check (date-year d2) => 2000)
) ;let


;; FIXME: strange?
(check-true (undefined? (date-year (make-time TIME-UTC 0 0))
            ) ;undefined?
) ;check-true


(check-report)
