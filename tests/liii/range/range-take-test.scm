(import (liii check) (liii range))


(check-set-mode! 'report-failed)


;; range-take
;; 从 range 开头提取指定数量的元素。
;;
;; 语法
;; ----
;; (range-take r count)
;;
;; 参数
;; ----
;; r : range
;; 源 range 对象。
;;
;; count : exact-natural
;; 要提取的元素数量。
;;
;; 返回值
;; ----
;; range
;; 新的 range 对象，包含前 count 个元素。
;;
;; 示例
;; ----
;; (range-take (numeric-range 0 10) 5) => 包含 0,1,2,3,4 的 range
;;
;; 错误处理
;; ----
;; 无


(let ((r (numeric-range 0 10)))
  (let ((taken (range-take r 5)))
    (check (range-length taken) => 5)
    (check (range-ref taken 0) => 0)
    (check (range-ref taken 4) => 4)
  ) ;let
) ;let


(let ((r (numeric-range 0 10)))
  (let ((taken (range-take r 3)))
    (check (range-length taken) => 3)
    (check (range-ref taken 0) => 0)
    (check (range-ref taken 2) => 2)
  ) ;let
) ;let


(let ((r (numeric-range 0 10)))
  (let ((taken (range-take r 0)))
    (check (range-length taken) => 0)
  ) ;let
) ;let


(check-report)
