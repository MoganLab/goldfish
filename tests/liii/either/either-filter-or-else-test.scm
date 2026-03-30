(import (liii check)
        (liii error)
        (liii either))

(check-set-mode! 'report-failed)

;; either-filter-or-else
;; 对 Right 值做条件过滤，不满足条件时转换为 Left。
;;
;; 语法
;; ----
;; (either-filter-or-else pred zero either)
;;
;; 参数
;; ----
;; pred : procedure?
;; 过滤条件。
;;
;; zero : any?
;; 过滤失败时写入 Left 的值。
;;
;; either : either
;; 输入 Either 值。
;;
;; 返回值
;; ----
;; either
;; Right 且满足 pred 时保持 Right；Right 且不满足时变为 Left(zero)；Left 时原样返回。
;;
;; 注意
;; ----
;; Left 值不会触发 pred。
;;
;; 示例
;; ----
;; (to-left (either-filter-or-else even? "err" (from-right 11))) => "err"
;;
;; 错误处理
;; ----
;; type-error 当 pred 不是过程或 either 不是 Either 时

(let ((r10 (from-right 10))
      (r11 (from-right 11))
      (l (from-left "orig")))
  (check (to-right (either-filter-or-else even? "err" r10)) => 10)

  (let ((res (either-filter-or-else even? "Must be even" r11)))
    (check-true (either-left? res))
    (check (to-left res) => "Must be even")
  ) ;let

  (let ((res-l (either-filter-or-else even? "Must be even" l)))
    (check-true (either-left? res-l))
    (check (to-left res-l) => "orig")
  ) ;let
) ;let

(check-catch 'type-error (either-filter-or-else even? 0 "not-either"))
(check-catch 'type-error (either-filter-or-else "not-a-proc" 0 (from-right 10)))

(check-report)
