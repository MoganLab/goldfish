;; decimal-divide-with-context
;; 计算两个 decimal 的商，按 math-context 的有效数字位数舍入。
;;
;; 语法
;; ----
;; (decimal-divide-with-context a b mc)
;;
;; 参数
;; ----
;; a : decimal?
;;   被除数。
;; b : decimal?
;;   除数。
;; mc : math-context?
;;   精度与舍入模式约束；precision=0 表示 UNLIMITED（scale 30）。
;;
;; 返回值
;; ----
;; decimal?
;;   按约束舍入后的商。

(import (liii check)
        (liii decimal)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(let ((mc (make-math-context 5 'half-up)))
  (check (decimal->string (decimal-divide-with-context
                            (d "10")
                            (d "3")
                            mc))
         => "3.3333")
) ;let

(check (decimal->string (decimal-divide-with-context
                          (d "100")
                          (d "3")
                          math-context-decimal32))
       => "33.33333"
) ;check

(let ((mc (make-math-context 5 'half-up)))
  (check (decimal->string (decimal-divide-with-context
                            (d "1")
                            (d "4")
                            mc))
         => "0.25")
) ;let

(check-report)
