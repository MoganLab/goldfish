;; call-with-stem-mode
;; 在 stem 模式下执行指定的 thunk，退出后恢复原有模式。
;;
;; 语法
;; ----
;; (call-with-stem-mode thunk)
;;
;; 参数
;; ----
;; thunk : procedure?
;; 不接受参数的procedure，在 stem 模式下被调用。
;;
;; 返回值
;; ----
;; any?
;; thunk 的返回值。
;;
;; 说明
;; ----
;; 使用临时绑定实现，thunk 抛出异常时模式同样会被正确恢复。

(import (liii check) (liii goldfmt scan normalize))

(check-set-mode! 'report-failed)

;; thunk 的返回值原样返回
(check (call-with-stem-mode (lambda () 42)) => 42)

;; thunk 内部处于 stem 模式
(check (call-with-stem-mode (lambda () (stem-mode?))) => #t)

;; thunk 退出后恢复原模式
(check (stem-mode?) => #f)

;; 嵌套调用时，内层退出后仍处于 stem 模式
(check
  (call-with-stem-mode
    (lambda () (call-with-stem-mode (lambda () (stem-mode?))) (stem-mode?))
  ) ;call-with-stem-mode
  =>
  #t
) ;check

;; 最外层退出后恢复为非 stem 模式
(check (stem-mode?) => #f)

(check-report)
