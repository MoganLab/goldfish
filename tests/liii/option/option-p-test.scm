(import (liii check)
        (liii option)
) ;import

(check-set-mode! 'report-failed)

;; option?
;; 判断值是否为 option 类型。
;;
;; 语法
;; ----
;; (option? x)
;;
;; 参数
;; ----
;; x : any
;; 要判断的值。
;;
;; 返回值
;; -----
;; 布尔值：
;; - #t 表示是 option 类型
;; - #f 表示不是 option 类型
;;
;; 说明
;; ----
;; 通过检查值的内部表示（cdr 是否为 'N 或 'S）来判断是否为 option。

(let ((opt1 (option 42))
      (opt2 (none)))
  (check (option? opt1) => #t)
  (check (option? opt2) => #t)
  (check (option? 42) => #f)
  (check (option? '()) => #f)
  (check (option? '(1 . 2)) => #f)
  (check (option? "hello") => #f)
) ;let

(check-report)
