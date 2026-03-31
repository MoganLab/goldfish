(import (liii check)
        (liii range)
) ;import

(check-set-mode! 'report-failed)

;; range-append
;; 连接多个 range。
;;
;; 语法
;; ----
;; (range-append)
;; (range-append r)
;; (range-append r1 r2)
;; (range-append r1 r2 ...)
;;
;; 参数
;; ----
;; r, r1, r2, ... : range
;; 要连接的 range 对象。
;;
;; 返回值
;; ----
;; range
;; 新的 range 对象，包含所有输入 range 的元素。
;;
;; 示例
;; ----
;; (range-append (numeric-range 0 3) (numeric-range 3 6)) => 包含 0-5 的 range
;;
;; 错误处理
;; ----
;; 无

(let ((r1 (numeric-range 0 3))
      (r2 (numeric-range 3 6)))
  (let ((appended (range-append r1 r2)))
    (check (range-length appended) => 6)
    (check (range->list appended) => '(0 1 2 3 4 5))
  ) ;let
) ;let

(let ((r1 (numeric-range 0 2))
      (r2 (numeric-range 2 4))
      (r3 (numeric-range 4 6)))
  (let ((appended (range-append r1 r2 r3)))
    (check (range-length appended) => 6)
    (check (range->list appended) => '(0 1 2 3 4 5))
  ) ;let
) ;let

(let ((r (numeric-range 0 5)))
  (let ((appended (range-append r)))
    (check (range-length appended) => 5)
    (check (range->list appended) => '(0 1 2 3 4))
  ) ;let
) ;let

(let ((r1 (numeric-range 0 0))
      (r2 (numeric-range 0 5)))
  (check (range->list (range-append r1 r2)) => '(0 1 2 3 4))
) ;let

(check-report)
