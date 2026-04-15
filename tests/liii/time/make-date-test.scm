(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; make-date
;; 创建日期对象。
;;
;; 语法
;; ----
;; (make-date nanosecond second minute hour day month year zone-offset)
;;
;; 参数
;; ----
;; nanosecond : integer?
;; second : integer?
;; minute : integer?
;; hour : integer?
;; day : integer?
;; month : integer?
;; year : integer?
;; zone-offset : integer?
;;
;; 返回值
;; ----
;; date? 一个新的日期对象。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数类型不正确时抛出错误。


;; Test make-date
(check-true (date? (make-date 0 0 0 0 1 1 1970 0))
) ;check-true
(check-true (date? (make-date 999999999
                     59
                     59
                     23
                     31
                     12
                     2023
                     28800
                   ) ;make-date
            ) ;date?
) ;check-true
(check-true (date? (make-date 500000000
                     30
                     30
                     12
                     15
                     6
                     2000
                     -14400
                   ) ;make-date
            ) ;date?
) ;check-true


;; Test edge cases
(check-true (date? (make-date 0 0 0 0 1 1 0 0))
) ;check-true
(check-true (date? (make-date 0 0 0 0 1 1 -1000 0))
) ;check-true
(check-true (date? (make-date 0 0 0 0 1 1 10000 0))
) ;check-true
(check-true (date? (make-date 0 0 0 0 1 1 2023 -64800)
            ) ;date?
) ;check-true
(check-true (date? (make-date 0 0 0 0 1 1 2023 64800)
            ) ;date?
) ;check-true


;; Test error conditions
(check-catch 'wrong-type-arg
  (make-date 'not-number 0 0 0 1 1 1970 0)
) ;check-catch
;; no range check
(check-true (date? (make-date -1 0 0 0 1 1 1970 0))
) ;check-true
(check-true (date? (make-date 1000000000 0 0 0 1 1 1970 0)
            ) ;date?
) ;check-true


(check-report)
