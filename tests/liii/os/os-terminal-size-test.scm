;; os-terminal-size
;; 获取终端窗口尺寸（列数与行数）。
;;
;; 语法
;; ----
;; (os-terminal-size)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; -----
;; pair? 或 #f
;; 返回 (cols . rows) 形式的点对；无法检测（如 CI 管道无 tty）时返回 #f。

(import (liii check) (liii os))

(check-set-mode! 'report-failed)

;; 返回 #f（无法检测，如 CI 无 tty）或 (cols rows) 正整数对
(let ((sz (os-terminal-size)))
  (check-true
    (or (not sz)
      (and (pair? sz)
        (pair? (cdr sz))
        (integer? (car sz))
        (> (car sz) 0)
        (integer? (cadr sz))
        (> (cadr sz) 0)
      ) ;and
    ) ;or
  ) ;check-true
) ;let

(check-report)
