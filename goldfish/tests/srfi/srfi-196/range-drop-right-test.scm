;; range-drop-right
;; 从 range 末尾删除指定数量的元素。
;;
;; 语法
;; ----
;; (range-drop-right r count)
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
;; 新的 range 对象，不包含后 count 个元素。
;;
;; 错误处理
;; ----
;; - type-error：当 r 不是 range，或 count 不是精确整数时抛出。
;; - value-error：当 count 为负或超出 range 长度时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (numeric-range 0 10)))
  (let ((dropped (range-drop-right r 3)))
    (check (range-length dropped) => 7)
    (check (range-ref dropped 0) => 0)
    (check (range-ref dropped 6) => 6)
  ) ;let
) ;let


(let ((r (numeric-range 0 10)))
  (let ((dropped (range-drop-right r 5)))
    (check (range-length dropped) => 5)
    (check (range-ref dropped 0) => 0)
    (check (range-ref dropped 4) => 4)
  ) ;let
) ;let


(let ((r (numeric-range 0 10)))
  (let ((dropped (range-drop-right r 0)))
    (check (range-length dropped) => 10)
    (check (range-ref dropped 9) => 9)
  ) ;let
) ;let



;; count 不是精确整数时抛出 type-error
(let ((r (numeric-range 0 10)))
  (check-catch 'type-error (range-drop-right r "3"))
  (check-catch 'type-error (range-drop-right r 1.5))
  (check-catch 'type-error (range-drop-right r 'n))
) ;let

;; count 为负或超出 range 长度时抛出 value-error
(let ((r (numeric-range 0 10)))
  (check-catch 'value-error (range-drop-right r -1))
  (check-catch 'value-error (range-drop-right r 11))
) ;let

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-drop-right 42 3))
(check-catch 'type-error (range-drop-right "r" 3))


(check-report)
