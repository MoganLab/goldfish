(import (liii check) (liii range))


(check-set-mode! 'report-failed)


;; range-drop
;; 从 range 开头删除指定数量的元素。
;;
;; 语法
;; ----
;; (range-drop r count)
;;
;; 参数
;; ----
;; r : range
;; 源 range 对象。
;;
;; count : exact-natural
;; 要删除的元素数量。
;;
;; 返回值
;; ----
;; range
;; 新的 range 对象，不包含前 count 个元素。
;;
;; 示例
;; ----
;; (range-drop (numeric-range 0 10) 5) => 包含 5,6,7,8,9 的 range
;;
;; 错误处理
;; ----
;; 无


(let ((r (numeric-range 0 10)))
  (let ((dropped (range-drop r 5)))
    (check (range-length dropped) => 5)
    (check (range-ref dropped 0) => 5)
    (check (range-ref dropped 4) => 9)
  ) ;let
) ;let


(let ((r (numeric-range 0 10)))
  (let ((dropped (range-drop r 3)))
    (check (range-length dropped) => 7)
    (check (range-ref dropped 0) => 3)
    (check (range-ref dropped 6) => 9)
  ) ;let
) ;let


(let ((r (numeric-range 0 10)))
  (let ((dropped (range-drop r 0)))
    (check (range-length dropped) => 10)
    (check (range-ref dropped 0) => 0)
  ) ;let
) ;let


(check-report)
