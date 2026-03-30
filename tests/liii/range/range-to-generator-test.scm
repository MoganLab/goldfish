(import (liii check)
        (srfi srfi-196)
) ;import

(check-set-mode! 'report-failed)

;; range->generator
;; 将 range 转换为生成器函数。
;;
;; 语法
;; ----
;; (range->generator r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; procedure
;; 生成器函数，每次调用返回下一个元素，结束时返回 eof-object。
;;
;; 示例
;; ----
;; (range->generator (numeric-range 0 5)) => 生成器函数
;;
;; 错误处理
;; ----
;; 无

(let ((r (numeric-range 0 5))
      (result '()))
  (let ((g (range->generator r)))
    (let loop ((v (g)))
      (if (eof-object? v)
          (check result => '(0 1 2 3 4))
          (begin
            (set! result (append result (list v)))
            (loop (g))
          ) ;begin
      ) ;if
    ) ;let
  ) ;let
) ;let

(let ((r (numeric-range 0 0)))
  (let ((g (range->generator r)))
    (check-true (eof-object? (g)))
  ) ;let
) ;let

(let ((r (numeric-range 1 4)))
  (let ((g (range->generator r)))
    (check (g) => 1)
    (check (g) => 2)
    (check (g) => 3)
    (check-true (eof-object? (g)))
  ) ;let
) ;let

(check-report)
