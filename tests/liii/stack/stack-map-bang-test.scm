(import (liii check) (liii stack))


;; stack-map!
;; 对栈中每个元素应用函数，修改原栈。
;;
;; 语法
;; ----
;; (stack-map! proc s)
;;
;; 参数
;; ----
;; proc : procedure?
;; 应用到每个元素的函数，接受一个参数，返回新值
;;
;; s : stack?
;; 要修改的栈
;;
;; 返回值
;; ----
;; stack?
;; 返回修改后的原栈
;;
;; 说明
;; ----
;; stack-map! 遍历栈中每个元素，将函数应用后结果替换原元素。
;; 元素顺序保持不变（从栈顶到栈底）。
;; 注意此函数会修改原栈，与 stack-map 不同。
;;
;; 错误处理
;; ----
;; type-error 当 proc 不是过程时
;; type-error 当 s 不是栈时


(let ((s (make-stack)))
  (stack-map! (lambda (x) (* x 2)) s)
  (check (stack-empty? s) => #t)
) ;let


(let ((s (stack 1)))
  (stack-map! (lambda (x) (* x 2)) s)
  (check (stack-top s) => 2)
) ;let


(let ((s (stack 1 2 3)))
  (stack-map! (lambda (x) (* x 2)) s)
  (check (stack->list s) => '(2 4 6))
) ;let


(let ((s (stack 1 2 3)))
  (stack-map! (lambda (x) (* x 2)) s)
  (check (stack->list s) => '(2 4 6))
) ;let


(let ((s (stack 1 2 3)))
  (check (stack-map! (lambda (x) (* x 2)) s)
    =>
    s
  ) ;check
) ;let


(let ((s (stack 10 20 30)))
  (stack-map! (lambda (x) (/ x 10)) s)
  (check (stack->list s) => '(1 2 3))
) ;let


(check-catch 'type-error
  (stack-map! "not-a-proc" (stack 1 2))
) ;check-catch
(check-catch 'type-error
  (stack-map! (lambda (x) x) 'not-a-stack)
) ;check-catch


(check-report)
