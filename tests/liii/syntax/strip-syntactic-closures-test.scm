(import (liii check) (liii syntax))

(check-set-mode! 'report-failed)

;; strip-syntactic-closures
;; 递归剥除表达式中的所有句法闭包包装。
;;
;; 语法
;; ----
;; (strip-syntactic-closures x)
;;
;; 参数
;; ----
;; x : any
;; 任意表达式，可包含句法闭包。
;;
;; 返回值
;; ------
;; any
;; 剥除所有句法闭包后的表达式；对 pair 和 vector 会递归处理，
;; 常量和非闭包对象原样返回。

;; 1. 非闭包对象原样返回
(check (strip-syntactic-closures 'x) => 'x)
(check (strip-syntactic-closures 42) => 42)
(check (strip-syntactic-closures '(a b c)) => '(a b c))
(check (strip-syntactic-closures #(1 2)) => #(1 2))

;; 2. 剥开顶层闭包
(check (strip-syntactic-closures (make-syntactic-closure (curlet) '() 'x)) => 'x)
(check (strip-syntactic-closures (make-syntactic-closure (curlet) '() '(+ 1 2))) => '(+ 1 2))

;; 3. 递归剥开列表内部的闭包
(let ((sc-a (make-syntactic-closure (curlet) '() 'a))
      (sc-b (make-syntactic-closure (curlet) '() 'b)))
  (check (strip-syntactic-closures (list sc-a 'c sc-b)) => '(a c b))
) ;let

;; 4. 递归剥开向量内部的闭包
(let ((sc (make-syntactic-closure (curlet) '() 'x)))
  (check (strip-syntactic-closures (vector sc 1 sc)) => #(x 1 x))
) ;let

;; 5. 多层嵌套闭包
(let* ((inner (make-syntactic-closure (curlet) '() 'z))
       (outer (make-syntactic-closure (curlet) '() inner)))
  (check (strip-syntactic-closures outer) => 'z)
) ;let*

;; 6. 嵌套结构中的深层闭包
(let ((sc (make-syntactic-closure (curlet) '() 'x)))
  (check (strip-syntactic-closures (list 'a (list sc) (vector sc))) => '(a (x) #(x)))
) ;let

(check-report)
