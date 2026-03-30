(import (liii check)
        (liii option)
) ;import

(check-set-mode! 'report-failed)

;; option-empty?
;; 判断 option 是否为空（none）。
;;
;; 语法
;; ----
;; (option-empty? opt)
;;
;; 参数
;; ----
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; 布尔值：
;; - #t 表示 option 为空
;; - #f 表示 option 包含值

(let ((opt1 (option 42))
      (opt2 (none)))
  (check (option-empty? opt1) => #f)
  (check (option-empty? opt2) => #t)
) ;let

(check-report)
