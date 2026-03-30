(import (liii check)
        (liii option)
) ;import

(check-set-mode! 'report-failed)

;; option-map
;; 对 option 中的值应用映射函数。
;;
;; 语法
;; ----
;; (option-map f opt)
;;
;; 参数
;; ----
;; f : procedure
;; 接受一个参数的映射函数。
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; 新的 option 对象：
;; - 如果原 option 为空：返回空 option
;; - 如果原 option 非空：返回包含 (f value) 的新 option

(let ((opt1 (option 42))
      (opt2 (none)))
  (check (option-map (lambda (x) (+ x 1)) opt1) => (option 43))
  (check (option-map (lambda (x) (+ x 1)) opt2) => (none))
  (check (option-map (lambda (x) (number->string x)) opt1) => (option "42"))
) ;let

(check-report)
