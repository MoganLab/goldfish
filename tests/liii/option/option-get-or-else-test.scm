(import (liii check)
        (liii option)
) ;import

(check-set-mode! 'report-failed)

;; option-get-or-else
;; 获取 option 中的值，如果为空则返回默认值。
;;
;; 语法
;; ----
;; (option-get-or-else default opt)
;;
;; 参数
;; ----
;; default : any or procedure
;; 默认值或返回默认值的函数。
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; option 中的值（如果非空）或默认值（如果为空）。

(let ((opt1 (option 42))
      (opt2 (none)))
  (check (option-get-or-else 0 opt1) => 42)
  (check (option-get-or-else 0 opt2) => 0)
  (check (option-get-or-else (lambda () "default") opt2) => "default")
) ;let

(check-report)
