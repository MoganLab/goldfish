;; recursive.scm: 递归树遍历与表达式求值中的调用栈回溯演示

(define (eval-expr expr env)
  (cond ((number? expr) expr)
        ((symbol? expr)
         (let ((val (assoc expr env)))
           (if val
             (cdr val)
             (error 'unbound-variable "variable not found in env: ~S" expr)
           ) ;if
         ) ;let
        ) ;
        ((pair? expr)
         (let ((op (car expr)) (args (cdr expr)))
           (case op
            ((+) (cons '+ (+ (eval-expr (car args) env) (eval-expr (cadr args) env))))
            ((*) (cons '* (* (eval-expr (car args) env) (eval-expr (cadr args) env))))
            ((/) (cons '/ (/ (eval-expr (car args) env) (eval-expr (cadr args) env))))
            (else (error 'syntax-error "unknown operator: ~S" op))
           ) ;case
         ) ;let
        ) ;
        (else (error 'type-error "invalid expression: ~S" expr))
  ) ;cond
) ;define

;; 构造一个嵌套表达式树：(+ (* 2 a) (/ 10 b))
;; 故意在环境 env 中缺少变量 'b，触发深层递归时的未绑定变量报错

(define ast '(+ (* 2 a) (/ 10 b)))

(define env '((a . 21)))

(eval-expr ast env)
