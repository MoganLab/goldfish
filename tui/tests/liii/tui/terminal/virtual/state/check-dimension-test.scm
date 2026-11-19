;; check-dimension
;; 校验终端尺寸参数必须为正整数。
;;
;; 语法
;; ----
;; (check-dimension n name)
;;
;; 参数
;; ----
;; n : any
;; 待校验的尺寸值。
;;
;; name : string?
;; 出错时用于标识调用点的名称。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui terminal virtual state))

(check-set-mode! 'report-failed)

;; 合法值不报错
(check (check-dimension 10 "width") => #<unspecified>)
(check (check-dimension 1 "width") => #<unspecified>)

;; 非整数报 type-error
(check-catch 'type-error (check-dimension "10" "width"))
(check-catch 'type-error (check-dimension 10.5 "width"))
(check-catch 'type-error (check-dimension #f "width"))

;; 非正数报 value-error
(check-catch 'value-error (check-dimension 0 "width"))
(check-catch 'value-error (check-dimension -3 "width"))

(check-report)
