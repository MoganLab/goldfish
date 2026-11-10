;; subrange
;; 提取 range 的子范围。
;;
;; 语法
;; ----
;; (subrange r start end)
;;
;; 参数
;; ----
;; r : range
;; 源 range 对象。
;;
;; start : exact-natural
;; 起始索引（包含）。
;;
;; end : exact-natural
;; 结束索引（不包含）。
;;
;; 返回值
;; ----
;; range
;; 新的 range 对象，包含从 start 到 end-1 的元素。
;;
;; 错误处理
;; ----
;; - type-error：当 r 不是 range，或 start、end 不是精确整数时抛出。
;; - value-error：当 start 或 end 为负、start 大于 end，或 end 超出 range 长度时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (numeric-range 0 10)))
  (let ((s (subrange r 2 7)))
    (check (range-length s) => 5)
    (check (range-ref s 0) => 2)
    (check (range-ref s 4) => 6)
  ) ;let
) ;let


(let ((r (numeric-range 0 10)))
  (let ((s (subrange r 0 5)))
    (check (range-length s) => 5)
    (check (range-ref s 0) => 0)
    (check (range-ref s 4) => 4)
  ) ;let
) ;let


(let ((r (numeric-range 0 10)))
  (let ((s (subrange r 5 10)))
    (check (range-length s) => 5)
    (check (range-ref s 0) => 5)
    (check (range-ref s 4) => 9)
  ) ;let
) ;let

;; start 与 end 相等时为空 range
(let ((r (numeric-range 0 10)))
  (check (range-length (subrange r 3 3)) => 0)
) ;let

;; start 或 end 不是精确整数时抛出 type-error
(let ((r (numeric-range 0 10)))
  (check-catch 'type-error (subrange r 'a 5))
  (check-catch 'type-error (subrange r 0 "5"))
  (check-catch 'type-error (subrange r 1.5 5))
) ;let

;; start 或 end 为负时抛出 value-error
(let ((r (numeric-range 0 10)))
  (check-catch 'value-error (subrange r -1 5))
  (check-catch 'value-error (subrange r 0 -5))
) ;let

;; start 大于 end 时抛出 value-error
(let ((r (numeric-range 0 10)))
  (check-catch 'value-error (subrange r 2 1))
) ;let

;; end 超出 range 长度时抛出 value-error
(let ((r (numeric-range 0 10)))
  (check-catch 'value-error (subrange r 0 11))
  (check-catch 'value-error (subrange r 5 20))
) ;let

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (subrange 42 0 5))
(check-catch 'type-error (subrange "r" 0 5))


(check-report)
