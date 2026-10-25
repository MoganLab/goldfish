(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; date-iso-week
;; 获取日期所在的 ISO 周数（1-53）。
;;
;; 语法
;; ----
;; (date-iso-week date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 周数（1 到 53）。
;;
;; 示例
;; ----
;; (date-iso-week (make-date 2025 10 14)) => 42
;; (date-iso-week (make-date 2008 12 29)) => 1
;; (date-iso-week (make-date 2010 1 2)) => 53
;;
;; 错误处理
;; --------
;; type-error 当参数不是日期对象时抛出错误。

(check (date-iso-week (make-date 2025 10 14)) => 42)
(check (date-iso-week (make-date 2008 12 29)) => 1)
(check (date-iso-week (make-date 2010 1 2)) => 53)

;; 1月1日为周五的年份（2016）：ISO 第 1 周从 1 月 4 日（周一）开始
(check (date-iso-week (make-date 2016 1 1)) => 53)
(check (date-iso-week (make-date 2016 1 4)) => 1)
(check (date-iso-week (make-date 2016 1 10)) => 1)
(check (date-iso-week (make-date 2016 6 1)) => 22)
(check (date-iso-week (make-date 2016 12 31)) => 52)

;; 1月1日为周六的年份（2022）：ISO 第 1 周从 1 月 3 日（周一）开始
(check (date-iso-week (make-date 2022 1 1)) => 52)
(check (date-iso-week (make-date 2022 1 2)) => 52)
(check (date-iso-week (make-date 2022 1 3)) => 1)
(check (date-iso-week (make-date 2022 1 9)) => 1)
(check (date-iso-week (make-date 2022 12 31)) => 52)

;; 1月1日为周日的年份（2023）：ISO 第 1 周从 1 月 2 日（周一）开始
(check (date-iso-week (make-date 2023 1 1)) => 52)
(check (date-iso-week (make-date 2023 1 2)) => 1)
(check (date-iso-week (make-date 2023 1 8)) => 1)
(check (date-iso-week (make-date 2023 12 31)) => 52)

;; 1 月初属于上一年最后一周的边界日期
(check (date-iso-week (make-date 1993 1 3)) => 53)
(check (date-iso-week (make-date 1994 1 1)) => 52)
(check (date-iso-week (make-date 2017 1 1)) => 52)

;; 其他回归用例
(check (date-iso-week (make-date 2024 2 29)) => 9)
(check (date-iso-week (make-date 2024 12 30)) => 1)
(check (date-iso-week (make-date 2020 12 31)) => 53)

;; 全范围回归：1970-2100 逐日与独立参考算法对比
;; 参考算法（Wikipedia ISO week date）：
;;   p = (ord - wd + 10) / 7，ord 为 1-based day-of-year，wd 为 1-7（周一=1）
;;   p = 0 时属上一年最后一周；p = 53 且 12-28 的 p < 53 时属下一年 W1
(define (test-day-of-year d)
  (let-values (((y m day) (date-ymd d)))
    (+ 1 (- (date->rata-die d) (date->rata-die (make-date y 1 1))))))

(define (test-iso-ref-week d)
  (let* ((y (date-year d))
         (ord (test-day-of-year d))
         (wd (date-iso-weekday d))
         (p (floor-quotient (+ (- ord wd) 10) 7)))
    (cond
      ((= p 0)
       (let* ((dec31 (make-date (- y 1) 12 31))
              (ord2 (test-day-of-year dec31))
              (wd2 (date-iso-weekday dec31)))
         (values (- y 1) (floor-quotient (+ (- ord2 wd2) 10) 7))))
      ((and (= p 53)
            (< (floor-quotient
                 (+ (- (test-day-of-year (make-date y 12 28))
                       (date-iso-weekday (make-date y 12 28)))
                    10)
                 7)
               53))
       (values (+ y 1) 1))
      (else (values y p)))))

(let loop ((rd (date->rata-die (make-date 1970 1 1)))
           (end (date->rata-die (make-date 2101 1 1))))
  (unless (= rd end)
    (let ((d (rata-die->date rd)))
      (let-values (((ref-y ref-w) (test-iso-ref-week d)))
        (unless (and (= (date-iso-week-year d) ref-y)
                     (= (date-iso-week d) ref-w))
          (error 'regression-failed
            "date-iso-week mismatch" d ref-y ref-w))))
    (loop (+ rd 1) end)))

(check-catch 'type-error (date-iso-week "2025-10-14"))
(check-catch 'type-error (date-iso-week 2025))

(check-report)
