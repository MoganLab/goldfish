(import (liii check) (liii string) (liii pi code core tools read))

(check-set-mode! 'report-failed)

;; pi-int-arg
;; 工具参数中的整数规范化：模型偶尔给出 1.0 这类浮点形式，
;; 数值上为整数的按整数处理；无法安全转换的给出清晰的参数错误。

;; 已经是整数：原样返回
(check (pi-int-arg 1 "offset") => 1)
(check (pi-int-arg 90 "limit") => 90)
(check (pi-int-arg 0 "offset") => 0)
(check (pi-int-arg -3 "offset") => -3)

;; 数值上为整数的浮点：按整数处理
(check (pi-int-arg 1.0 "offset") => 1)
(check (pi-int-arg 90.0 "limit") => 90)
(check (pi-int-arg 0.0 "offset") => 0)
(check (pi-int-arg -2.0 "offset") => -2)
(check (pi-int-arg 1e2 "limit") => 100)

;; 结果必须是精确整数（integer? 为 #t）
(check-true (integer? (pi-int-arg 1.0 "offset")))
(check-true (exact-integer? (pi-int-arg 1.0 "offset")))

;; 非整数值浮点：报参数错误，而不是继续往下传
(check-catch 'error (pi-int-arg 1.5 "offset"))
(check-catch 'error (pi-int-arg 90.5 "limit"))
(check-catch 'error (pi-int-arg -1.5 "offset"))

;; 非数字：报参数错误
(check-catch 'error (pi-int-arg "1" "offset"))
(check-catch 'error (pi-int-arg #f "offset"))

;; 超出精确整数范围（2^63-1）：报参数错误
(check-catch 'error (pi-int-arg 1e30 "offset"))

;; 错误信息包含参数名，便于定位
(let ((msg (catch 'error
               (lambda () (pi-int-arg 1.5 "offset"))
               (lambda (tag . info)
                 (if (and (pair? info) (pair? (car info))) (caar info) "")))) )
  (check-true (string-contains? msg "offset"))
) ;let

(let ((msg (catch 'error
               (lambda () (pi-int-arg 1.5 "limit"))
               (lambda (tag . info)
                 (if (and (pair? info) (pair? (car info))) (caar info) "")))) )
  (check-true (string-contains? msg "limit"))
) ;let

(check-report)
