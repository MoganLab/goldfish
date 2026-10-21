(import (liii check) (liii os))

(check-set-mode! 'report-failed)

;; os-terminal-size 返回 #f（无法检测，如 CI 无 tty）或 (cols rows) 正整数对
(let ((sz (os-terminal-size)))
  (check-true (or (not sz)
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
