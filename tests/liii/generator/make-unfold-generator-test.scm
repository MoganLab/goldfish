(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; make-unfold-generator
;; 通过 unfold 算法模式从种子状态逐步生成序列的生成器。
;;
;; 语法
;; ----
;; (make-unfold-generator stop? mapper successor seed)
;;
;; 参数
;; ----
;; stop? : procedure
;; 判定当前状态是否停止生成的单参谓词。
;;
;; mapper : procedure
;; 将当前状态映射为生成器产出值的单参过程。
;;
;; successor : procedure
;; 由当前状态计算下一状态的单参过程。
;;
;; seed : any
;; 初始种子状态。
;;
;; 返回值
;; ----
;; procedure
;; 一个无参生成器过程。产出展开得到的值，并在满足 stop? 时返回 eof-object。
;;
;; 错误处理
;; ----
;; 无

(let ((g (make-unfold-generator
           (lambda (x) (>= x 3))
           (lambda (x) x)
           (lambda (x) (+ x 1))
           0)))
  (check (g) => 0)
  (check (g) => 1)
  (check (g) => 2)
  (check-true (eof-object? (g)))
)

(let ((g (make-unfold-generator
           null?
           car
           cdr
           '(a b))))
  (check (g) => 'a)
  (check (g) => 'b)
  (check-true (eof-object? (g)))
)

(check-report)
