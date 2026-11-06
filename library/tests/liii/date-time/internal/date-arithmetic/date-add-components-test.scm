;; date-add-components
;; 按年、月、周、日分量对日期做算术加法，日期溢出时截断到月末。
;;
;; 语法
;; ----
;; (date-add-components date y-plus m-plus w-plus d-plus)
;;
;; 参数
;; ----
;; date : date? 起始日期。
;; y-plus : integer? 增加的年数。
;; m-plus : integer? 增加的月数。
;; w-plus : integer? 增加的周数。
;; d-plus : integer? 增加的天数。
;;
;; 返回值
;; ----
;; date? 计算后的新日期。

(import (liii check)
  (liii date-time date)
  (liii date-time internal date-arithmetic)
) ;import

(check-set-mode! 'report-failed)

(check
  (let-values (((y m d) (date-ymd (date-add-components (make-date 2021 1 31) 0 1 0 0))))
    (list y m d)
  ) ;let-values
  =>
  '(2021 2 28)
) ;check
(check
  (let-values (((y m d) (date-ymd (date-add-components (make-date 2021 1 31) 1 13 1 2))))
    (list y m d)
  ) ;let-values
  =>
  '(2023 3 9)
) ;check
(check
  (let-values (((y m d) (date-ymd (date-add-components (make-date 2020 1 31) 0 1 0 0))))
    (list y m d)
  ) ;let-values
  =>
  '(2020 2 29)
) ;check

(check-report)
