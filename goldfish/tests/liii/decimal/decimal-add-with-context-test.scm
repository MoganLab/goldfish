;; decimal-add-with-context
;; 计算两个 decimal 的和，并按 math-context 约束结果精度。
;;
;; 语法
;; ----
;; (decimal-add-with-context a b mc)
;;
;; 参数
;; ----
;; a : decimal?
;;   第一个操作数。
;; b : decimal?
;;   第二个操作数。
;; mc : math-context?
;;   精度与舍入模式约束。
;;
;; 返回值
;; ----
;; decimal?
;;   按约束舍入后的和。

(import (liii check)
        (liii decimal)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(let ((mc (make-math-context 5 'half-up)))
  (check (decimal->string (decimal-add-with-context
                            (d "1.2345678")
                            (d "1.1111111")
                            mc))
         => "2.3457")
) ;let

(let ((mc math-context-unlimited))
  (check (decimal->string (decimal-add-with-context
                            (d "1.23456789")
                            (d "1.11111111")
                            mc))
         => "2.34567900")
) ;let

;; 大数有效数字舍入（BUG-6）
(let ((mc (make-math-context 7 'half-even)))
  (check (decimal->string (decimal-add-with-context
                            (d "12345678.9")
                            (d "0")
                            mc))
         => "12345680")
) ;let

(check-report)
