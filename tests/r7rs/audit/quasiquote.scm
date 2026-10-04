(import (scheme base) (scheme eval) (scheme write) (liii check))
(check-set-mode! 'report-failed)
(include "../fixtures/semantic-audit.scm")
(audit-check 'qq.constant (lambda () (audit-eval '(quasiquote (a #(b c) . d)))) '(a #(b c) . d))
(audit-check 'qq.unquote (lambda () (audit-eval '(quasiquote (list (unquote (+ 1 2)) 4)))) '(list 3 4))
(audit-check 'qq.list-splice
  (lambda () (audit-eval '(quasiquote (a (unquote-splicing (list 1 2)) b (unquote-splicing '()) c)))) '(a 1 2 b c))
(audit-check 'qq.dotted-tail
  (lambda () (audit-eval '(let ((x 42)) (quasiquote (a . (unquote x)))))) '(a . 42))
(audit-check 'qq.splice-dotted-tail
  (lambda () (audit-eval '(quasiquote ((unquote-splicing (list 1 2)) . (unquote (cons 3 4)))))) '(1 2 3 . 4))
(audit-check 'qq.vector-splice
  (lambda () (audit-eval '(quasiquote #(a (unquote (+ 1 2)) (unquote-splicing (list 4 5)) b)))) '#(a 3 4 5 b))
(audit-check 'qq.nesting
  (lambda () (audit-eval '(quasiquote (a (quasiquote (b (unquote (+ 1 2)) (unquote (unquote (+ 3 4)))))))))
  '(a (quasiquote (b (unquote (+ 1 2)) (unquote 7)))))
(audit-check 'qq.once
  (lambda () (audit-eval '(let ((n 0))
                           (let ((x (quasiquote ((unquote (begin (set! n (+ n 1)) n))))))
                             (list x n))))) '((1) 1))
(audit-check 'qq.hygiene
  (lambda () (audit-eval '(let ((cons #f) (append #f) (list #f) (vector #f))
                           (quasiquote (a (unquote (+ 1 2))))))) '(a 3))
(check-report)
