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
;; 错误处理
;; ----
;; - type-error：当 r 不是 range，或 count 不是精确整数时抛出。
;; - value-error：当 count 为负或超出 range 长度时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




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



;; count 不是精确整数时抛出 type-error
(let ((r (numeric-range 0 10)))
  (check-catch 'type-error (range-drop r "3"))
  (check-catch 'type-error (range-drop r 1.5))
  (check-catch 'type-error (range-drop r 'n))
) ;let

;; count 为负或超出 range 长度时抛出 value-error
(let ((r (numeric-range 0 10)))
  (check-catch 'value-error (range-drop r -1))
  (check-catch 'value-error (range-drop r 11))
) ;let

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-drop 42 3))
(check-catch 'type-error (range-drop "r" 3))


(check-report)
