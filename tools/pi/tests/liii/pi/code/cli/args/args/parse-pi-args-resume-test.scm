(import (liii check)
  (liii pi code cli args)
) ;import

(check-set-mode! 'report-failed)

;; --resume / -r 参数解析

;; 1. 长短选项均解析为 #t
(let ((res1 (parse-pi-args '("-r")))
      (res2 (parse-pi-args '("--resume"))))
  (check (pi-args-ref res1 'resume) => #t)
  (check (pi-args-ref res2 'resume) => #t))

;; 2. 默认未指定为 #f
(check (pi-args-ref (parse-pi-args '()) 'resume) => #f)

(check-report)
