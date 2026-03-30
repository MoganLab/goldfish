(import (liii check)
        (liii option)
) ;import

(check-set-mode! 'report-failed)

;; option=?
;; 比较两个 option 是否相等。
;;
;; 语法
;; ----
;; (option=? opt1 opt2)
;;
;; 参数
;; ----
;; opt1 : option
;; 第一个 option 对象。
;; opt2 : option
;; 第二个 option 对象。
;;
;; 返回值
;; -----
;; 布尔值：
;; - #t 如果两个 option 都为空，或都包含相等的值（使用 equal? 比较）
;; - #f 否则

(let ((opt1 (option 42))
      (opt2 (option 42))
      (opt3 (option "hello"))
      (opt4 (none))
      (opt5 (none)))
  (check (option=? opt1 opt2) => #t)
  (check (option=? opt1 opt3) => #f)
  (check (option=? opt4 opt5) => #t)
  (check (option=? opt1 opt4) => #f)
) ;let

(check-report)
