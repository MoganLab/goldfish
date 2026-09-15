(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gmerge
;; 根据比较谓词将多个递增有序的生成器合并为一个保持有序的新生成器。
;;
;; 语法
;; ----
;; (gmerge < gen ...)
;;
;; 参数
;; ----
;; < : procedure
;; 严格小于比较谓词，接收两参并返回布尔值。
;;
;; gen ... : procedure
;; 一个或多个输入生成器，假定每个生成器已按 < 排序递增。
;;
;; 返回值
;; ----
;; procedure
;; 新生成器过程。每次返回所有生成器当前元素中的最小值。
;;
;; 错误处理
;; ----
;; 若未提供生成器参数，会抛出错误。

(let ((g (gmerge < (generator 1 3 5) (generator 2 4 6))))
  (check (generator->list g) => '(1 2 3 4 5 6))
)

(let ((g (gmerge > (generator 1 3 5) (generator 2 4 6))))
  (check (generator->list g) => '(2 4 6 1 3 5))
)

(let ((g (gmerge char<? (generator #\a #\b #\c) (generator #\d #\e #\f))))
  (check (generator->list g) => '(#\a #\b #\c #\d #\e #\f))
)

(check-report)
