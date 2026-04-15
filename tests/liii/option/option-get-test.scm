(import (liii check) (liii option))


(check-set-mode! 'report-failed)


;; option-get
;; 获取 option 中的值，如果为空则报错。
;;
;; 语法
;; ----
;; (option-get opt)
;;
;; 参数
;; ----
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; option 中的值。
;;
;; 异常
;; ----
;; 如果 option 为空，抛出错误。


(let ((opt1 (option 42)))
  (check (option-get opt1) => 42)
) ;let


(check-report)
