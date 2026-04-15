(import (liii check) (liii range))


(check-set-mode! 'report-failed)


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
;; 示例
;; ----
;; (subrange (numeric-range 0 10) 2 7) => 包含 2,3,4,5,6 的 range
;;
;; 错误处理
;; ----
;; 无


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


(check-report)
