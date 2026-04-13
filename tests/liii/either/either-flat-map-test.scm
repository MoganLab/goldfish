(import (liii check)
        (liii error)
        (liii either)
) ;import

(check-set-mode! 'report-failed)

;; either-flat-map
;; 对 Right 值执行返回 Either 的过程，并避免产生嵌套 Either。
;;
;; 语法
;; ----
;; (either-flat-map proc either)
;;
;; 参数
;; ----
;; proc : procedure?
;; 要应用到 Right 值上的函数，返回值应为 Either。
;;
;; either : either
;; 输入 Either 值。
;;
;; 返回值
;; ----
;; either
;; 若输入为 Right，则返回 proc 的结果；若输入为 Left，则原样返回。
;;
;; 注意
;; ----
;; 对 Left 具有短路特性，不会执行 proc。
;;
;; 示例
;; ----
;; (to-right (either-flat-map (lambda (x) (from-right (* x 2))) (from-right 5))) => 10
;;
;; 错误处理
;; ----
;; type-error 当 proc 不是过程或 either 不是 Either 时

(let ((left-val (from-left "error"))
      (right-val (from-right 5)))
  (check (to-left (either-flat-map (lambda (x) (from-right (* x 2))) left-val)) => "error")
  (let ((result (either-flat-map (lambda (x) (from-right (* x 2))) right-val)))
    (check-true (either-right? result))
    (check (to-right result) => 10)
  ) ;let
) ;let

(let* ((val1 (from-right 10))
       (val2 (either-flat-map (lambda (x) (from-right (+ x 5))) val1))
       (val3 (either-flat-map (lambda (x) (from-right (* x 2))) val2)))
  (check-true (either-right? val3))
  (check (to-right val3) => 30)
) ;let*

(let ((result (either-flat-map (lambda (x) (from-left (string-append "bad: " (number->string x))))
                               (from-right 7))))
  (check-true (either-left? result))
  (check (to-left result) => "bad: 7")
) ;let

(check-catch 'type-error (either-flat-map (lambda (x) (from-right x)) "not-either"))
(check-catch 'type-error (either-flat-map "not-a-proc" (from-right 10)))

(check-report)
