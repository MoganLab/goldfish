(import (liii check)
        (liii error)
        (liii either)
) ;import

(check-set-mode! 'report-failed)

;; either-map
;; 对 Right 值应用映射函数，对 Left 值保持原样。
;;
;; 语法
;; ----
;; (either-map proc either)
;;
;; 参数
;; ----
;; proc : procedure?
;; 要应用到 Right 值上的函数。
;;
;; either : either
;; 输入 Either 值。
;;
;; 返回值
;; ----
;; either
;; 若输入为 Right，则返回映射后的 Right；若输入为 Left，则原样返回。
;;
;; 注意
;; ----
;; 对 Left 进行映射时具有短路特性，不会执行 proc。
;;
;; 示例
;; ----
;; (to-right (either-map (lambda (x) (* x 2)) (from-right 5))) => 10
;;
;; 错误处理
;; ----
;; type-error 当 proc 不是过程或 either 不是 Either 时

(let ((left-val (from-left "error"))
      (right-val (from-right 5)))
  (check (to-left (either-map (lambda (x) (* x 2)) left-val)) => "error")
  (let ((result (either-map (lambda (x) (* x 2)) right-val)))
    (check-true (either-right? result))
    (check (to-right result) => 10)
  ) ;let
) ;let

(let* ((val1 (from-right 10))
       (val2 (either-map (lambda (x) (+ x 5)) val1))
       (val3 (either-map (lambda (x) (* x 2)) val2)))
  (check-true (either-right? val3))
  (check (to-right val3) => 30)
) ;let*

(let* ((error-val (from-left "network error"))
       (mapped-error (either-map (lambda (x) (string-append "Error: " x)) error-val)))
  (check-true (either-left? mapped-error))
  (check (to-left mapped-error) => "network error")
) ;let*

(check-catch 'type-error (either-map (lambda (x) x) "not-either"))
(check-catch 'type-error (either-map "not-a-proc" (from-right 10)))

(check-report)
