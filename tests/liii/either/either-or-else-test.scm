(import (liii check)
        (liii error)
        (liii either)
) ;import

(check-set-mode! 'report-failed)

;; either-or-else
;; 提供 Either 级别的备选方案。
;;
;; 语法
;; ----
;; (either-or-else either backup)
;;
;; 参数
;; ----
;; either : either
;; 主 Either 值。
;;
;; backup : either
;; 当主值为 Left 时返回的备选 Either。
;;
;; 返回值
;; ----
;; either
;; 若主值为 Right 则返回主值，否则返回 backup。
;;
;; 注意
;; ----
;; 该函数返回的是 Either，而不是其中包裹的值。
;;
;; 示例
;; ----
;; (to-right (either-or-else (from-left 0) (from-right 2))) => 2
;;
;; 错误处理
;; ----
;; type-error 当 either 不是 Either 时

(let ((main (from-right 1))
      (backup (from-right 2))
      (fail (from-left 0)))
  (check (to-right (either-or-else main backup)) => 1)
  (check (to-right (either-or-else fail backup)) => 2)
) ;let

(check-catch 'type-error (either-or-else "not-either" (from-right 1)))

(check-report)
