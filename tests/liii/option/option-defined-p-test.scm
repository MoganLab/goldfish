(import (liii check) (liii option))


(check-set-mode! 'report-failed)


;; option-defined?
;; 判断 option 是否包含值。
;;
;; 语法
;; ----
;; (option-defined? opt)
;;
;; 参数
;; ----
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; 布尔值：
;; - #t 表示 option 包含值
;; - #f 表示 option 为空


(let ((opt1 (option 42)) (opt2 (none)))
  (check (option-defined? opt1) => #t)
  (check (option-defined? opt2) => #f)
) ;let


(check-report)
