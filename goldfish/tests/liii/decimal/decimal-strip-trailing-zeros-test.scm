;; decimal-strip-trailing-zeros
;; 去除 decimal 末尾多余的零（减小 scale）。
;;
;; 语法
;; ----
;; (decimal-strip-trailing-zeros n)
;;
;; 参数
;; ----
;; n : decimal?
;;   输入 decimal。
;;
;; 返回值
;; ----
;; decimal?
;;   去除尾随零后的 decimal；零值返回 scale=0。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

;; 基本测试：去除尾随零
(check (decimal->string (decimal-strip-trailing-zeros (d "1.2000"))) => "1.2")
(check (decimal-scale (decimal-strip-trailing-zeros (d "1.2000"))) => 1)

;; 整数部分 + 小数尾随零
(check (decimal->string (decimal-strip-trailing-zeros (d "123.45000"))) => "123.45")
(check (decimal-scale (decimal-strip-trailing-zeros (d "123.45000"))) => 2)

;; 只有整数部分（无尾随零可去）
(check (decimal->string (decimal-strip-trailing-zeros (d "123"))) => "123")
(check (decimal-scale (decimal-strip-trailing-zeros (d "123"))) => 0)

;; 纯小数（第一个非零数字后）
(check (decimal->string (decimal-strip-trailing-zeros (d "0.0012300"))) => "0.00123")
(check (decimal-scale (decimal-strip-trailing-zeros (d "0.0012300"))) => 5)

;; 零值应返回 scale=0
(check (decimal->string (decimal-strip-trailing-zeros (d "0.0000"))) => "0")

(check-report)
