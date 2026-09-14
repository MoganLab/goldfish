(import (scheme base) (scheme write))
;; A tiny meta-circular evaluator (quote/if/lambda/call + numbers) run
;; under BOTH engines: gf0 evaluating an evaluator. All helpers live in
;; ONE toplevel letrec (this expander rejects toplevel forward refs, so
;; mutual recursion must be local -- good exercise for letrec at scale).
(define meta-results
  (letrec
    ((m-eval
       (lambda (expr env)
         (cond ((number? expr) expr)
               ((symbol? expr) (m-lookup expr env))
               ((eq? (car expr) 'quote) (car (cdr expr)))
               ((eq? (car expr) 'if)
                (if (m-eval (car (cdr expr)) env)
                  (m-eval (car (cdr (cdr expr))) env)
                  (m-eval (car (cdr (cdr (cdr expr)))) env)))
               ((eq? (car expr) 'lambda) (list 'm-closure expr env))
               (else (m-apply (m-eval (car expr) env)
                              (m-evlis (cdr expr) env))))))
     (m-lookup
       (lambda (sym env)
         (if (null? env) 'UNBOUND
           (if (eq? sym (car (car env))) (cdr (car env))
             (m-lookup sym (cdr env))))))
     (m-evlis
       (lambda (xs env)
         (if (null? xs) '()
           (cons (m-eval (car xs) env) (m-evlis (cdr xs) env)))))
     (m-apply
       (lambda (proc args)
         (if (eq? proc 'prim+) (prim+-apply args)
           (if (eq? proc 'prim*) (prim*-apply args)
             (if (eq? (car proc) 'm-closure)
               (m-eval (car (cdr (cdr (car (cdr proc)))))
                       (m-bind (car (cdr (car (cdr proc)))) args
                               (car (cdr (cdr proc)))))
               'UNKNOWN-PRIM)))))
     (m-bind
       (lambda (formals args env)
         (if (null? formals) env
           (cons (cons (car formals) (car args))
                 (m-bind (cdr formals) (cdr args) env)))))
     (prim+-apply
       (lambda (args)
         (if (null? args) 0 (+ (car args) (prim+-apply (cdr args))))))
     (prim*-apply
       (lambda (args)
         (if (null? args) 1 (* (car args) (prim*-apply (cdr args))))))
     (base-env
       (list (cons '+ 'prim+) (cons '* 'prim*))))
    (list (m-eval '(+ 1 (* 2 3)) base-env)
          (m-eval '((lambda (x y) (+ x y)) 10 20) base-env)
          (m-eval '(if 0 'yes 'no) base-env)
          (m-eval '((lambda (f) (f 5)) (lambda (z) (* z z))) base-env)
          (m-eval '(quote (a b c)) base-env))))
(display "m: ")
(display meta-results)
(newline)
