(import (liii check) (srfi srfi-165))

(check-set-mode! 'report-failed)




(define test-var-1
  (make-computation-environment-variable 'test-var-1 "default" #f)
) ;define
(define test-var-2 (make-computation-environment-variable 'test-var-2 42 #t))
(define test-var-1*
  (make-computation-environment-variable 'test-var-1 "default" #f)
) ;define

(check-false (eq? test-var-1 test-var-1*))




(define env-1 (make-computation-environment))
(define env-2 (make-computation-environment))

(check-true (vector? env-1))
(check-false (eq? env-1 env-2))

(check (computation-environment-ref env-1 default-computation) => #f)




(define var-x (make-computation-environment-variable 'x 100 #f))
(define var-y (make-computation-environment-variable 'y 200 #f))

(check (computation-environment-ref env-1 var-x) => 100)
(check (computation-environment-ref env-1 var-y) => 200)

(check-catch 'wrong-type-arg (computation-environment-ref 'not-env var-x))



(define env-updated (computation-environment-update env-1 var-x 999))

(check (computation-environment-ref env-1 var-x) => 100)

(check (computation-environment-ref env-updated var-x) => 999)

(define env-multi (computation-environment-update env-1 var-x 1 var-y 2))
(check (computation-environment-ref env-multi var-x) => 1)
(check (computation-environment-ref env-multi var-y) => 2)

(check (let ((global-env (begin (computation-environment-update! env-1 var-x 'global) env-1))
            ) ;
         (computation-environment-ref (computation-environment-update global-env var-x 'local)
           var-x
         ) ;computation-environment-ref
       ) ;let
  =>
  'local
) ;check




(define env-mutable (make-computation-environment))

(computation-environment-update! env-mutable var-x 'modified)
(check (computation-environment-ref env-mutable var-x) => 'modified)

(define dynamic-var
  (make-computation-environment-variable 'dynamic 'initial #f)
) ;define
(computation-environment-update! env-mutable dynamic-var 'new-value)
(check (computation-environment-ref env-mutable dynamic-var) => 'new-value)

(computation-environment-update! env-mutable var-x 'first)
(computation-environment-update! env-mutable var-x 'second)
(check (computation-environment-ref env-mutable var-x) => 'second)





(check (computation-run (make-computation (lambda (compute) (compute (computation-pure 42))))
       ) ;computation-run
  =>
  42
) ;check

(let ((c (make-computation (lambda (compute) (compute (computation-pure 'first))))))
  (check (computation-run c) => 'first)
  (check (computation-run c) => 'first)
) ;let

(check (let ((test-var (make-computation-environment-variable 'test 100 #f)))
         (computation-run (computation-with ((test-var 999))
                            (make-computation (lambda (compute)
                                                (compute (computation-fn ((v test-var)) (computation-pure v)))
                                              ) ;lambda
                            ) ;make-computation
                          ) ;computation-with
         ) ;computation-run
       ) ;let
  =>
  999
) ;check

(check (computation-run (make-computation (lambda (compute)
                                            (compute (make-computation (lambda (compute2) (compute2 (computation-pure 'nested))))
                                            ) ;compute
                                          ) ;lambda
                        ) ;make-computation
       ) ;computation-run
  =>
  'nested
) ;check

(check (computation-run (computation-bind (make-computation (lambda (compute) (compute (computation-pure 10))))
                          (lambda (x) (computation-pure (* x 2)))
                        ) ;computation-bind
       ) ;computation-run
  =>
  20
) ;check





(check (computation-run (computation-pure 'test)) => 'test)

(check (let ((counter (make-computation-environment-variable 'counter 0 #f)))
         (computation-run (computation-bind (computation-with! (counter 1))
                            (lambda (_) (computation-fn ((c counter)) (computation-pure c)))
                          ) ;computation-bind
         ) ;computation-run
       ) ;let
  =>
  1
) ;check

(check (let ((counter (make-computation-environment-variable 'counter 0 #f)))
         (computation-run (computation-each (computation-with! (counter 2))
                            (computation-fn ((c counter)) (computation-pure c))
                          ) ;computation-each
         ) ;computation-run
       ) ;let
  =>
  2
) ;check

(check (computation-run (computation-fn ((c (make-computation-environment-variable 'counter 0 #f)))
                          (computation-pure c)
                        ) ;computation-fn
       ) ;computation-run
  =>
  0
) ;check




(define var-ask (make-computation-environment-variable 'ask 42 #f))

(check (computation-run (computation-bind (computation-ask)
                          (lambda (env) (computation-pure (computation-environment-ref env var-ask)))
                        ) ;computation-bind
       ) ;computation-run
  =>
  42
) ;check




(define var-local (make-computation-environment-variable 'local 'global-val #f))

(check (computation-run (computation-local (lambda (env) (computation-environment-update env var-x 'local-val))
                          (computation-bind (computation-ask)
                            (lambda (e) (computation-pure (computation-environment-ref e var-x)))
                          ) ;computation-bind
                        ) ;computation-local
       ) ;computation-run
  =>
  'local-val
) ;check

(check (computation-environment-ref env-1 var-local) => 'global-val)



(check (computation-run (computation-pure 42)) => 42)
(check (computation-run (computation-pure 'hello)) => 'hello)

(check (call-with-values (lambda () (computation-run (computation-pure 1 2 3))) list)
  =>
  '(1 2 3)
) ;check




(check (computation-run (computation-each (computation-pure 1)
                          (computation-pure 2)
                          (computation-pure 3)
                        ) ;computation-each
       ) ;computation-run
  =>
  3
) ;check

(check (let ((result '()))
         (computation-run (computation-each (make-computation (lambda (k) (set! result (cons 1 result)) (k (computation-pure 'void)))
                                            ) ;make-computation
                            (make-computation (lambda (k) (set! result (cons 2 result)) (k (computation-pure 'void)))
                            ) ;make-computation
                            (make-computation (lambda (k) (set! result (cons 3 result)) (k (computation-pure 'void)))
                            ) ;make-computation
                          ) ;computation-each
         ) ;computation-run
         result
       ) ;let
  =>
  '(3 2 1)
) ;check




(check (computation-run (computation-each-in-list (list (computation-pure 'a) (computation-pure 'b) (computation-pure 'c))
                        ) ;computation-each-in-list
       ) ;computation-run
  =>
  'c
) ;check



(check (computation-run (computation-bind (computation-pure 5) (lambda (x) (computation-pure (* x 2))))
       ) ;computation-run
  =>
  10
) ;check

(check (computation-run (computation-bind (computation-pure 1 2)
                          (lambda (a b) (computation-pure (+ a b)))
                        ) ;computation-bind
       ) ;computation-run
  =>
  3
) ;check

(check (computation-run (computation-bind (computation-pure 10)
                          (lambda (x)
                            (computation-bind (computation-pure 20) (lambda (y) (computation-pure (+ x y))))
                          ) ;lambda
                        ) ;computation-bind
       ) ;computation-run
  =>
  30
) ;check




(check (computation-run (computation-sequence (list (computation-pure 1) (computation-pure 2) (computation-pure 3))
                        ) ;computation-sequence
       ) ;computation-run
  =>
  '(1 2 3)
) ;check

(check (computation-run (computation-sequence '())) => '())

(check (computation-run (computation-sequence (list (computation-pure 'a) (computation-pure 'b)))
       ) ;computation-run
  =>
  '(a b)
) ;check

(check (let ((n 0))
         (computation-run (computation-sequence (list (make-computation (lambda (k) (set! n (+ n 1)) (k (computation-pure n))))
                                                  (make-computation (lambda (k) (set! n (+ n 1)) (k (computation-pure n))))
                                                  (make-computation (lambda (k) (set! n (+ n 1)) (k (computation-pure n))))
                                                ) ;list
                          ) ;computation-sequence
         ) ;computation-run
         n
       ) ;let
  =>
  3
) ;check



(check (computation-run (computation-with ((var-x 'shared))
                          (computation-forked (computation-with! (var-x 'branch1))
                            (computation-fn ((x var-x)) (computation-pure x))
                          ) ;computation-forked
                        ) ;computation-with
       ) ;computation-run
  =>
  'shared
) ;check

(check (computation-run (computation-with ((var-x 0))
                          (computation-forked (computation-with! (var-x 999)) (computation-pure 'done))
                          (computation-fn ((x var-x)) (computation-pure x))
                        ) ;computation-with
       ) ;computation-run
  =>
  0
) ;check




(check (computation-run (computation-with ((var-x 'original))
                          (computation-bind/forked (computation-with! (var-x 'changed))
                            (lambda (_) (computation-fn ((x var-x)) (computation-pure x)))
                          ) ;computation-bind/forked
                        ) ;computation-with
       ) ;computation-run
  =>
  'original
) ;check




(check (computation-run (computation-fn ((x var-x)) (computation-pure x)))
  =>
  100
) ;check

(check (computation-run (computation-fn (var-x) (computation-pure var-x)))
  =>
  100
) ;check

(check (computation-run (computation-fn ((x var-x) (y var-y))
                          (let ((sum (+ x y)))
                            (computation-pure sum)
                          ) ;let
                        ) ;computation-fn
       ) ;computation-run
  =>
  300
) ;check




(check (computation-run (computation-with ((var-x 999))
                          (computation-fn ((x var-x)) (computation-pure x))
                        ) ;computation-with
       ) ;computation-run
  =>
  999
) ;check

(check (computation-run (computation-with ((var-x 1) (var-y 2))
                          (computation-each (computation-pure 'ignored))
                          (computation-fn ((x var-x) (y var-y)) (computation-pure (list x y)))
                        ) ;computation-with
       ) ;computation-run
  =>
  '(1 2)
) ;check

(check (computation-run (computation-with ((var-x 'outer))
                          (computation-with ((var-x 'inner))
                            (computation-fn ((x var-x)) (computation-pure x))
                          ) ;computation-with
                        ) ;computation-with
       ) ;computation-run
  =>
  'inner
) ;check




(check (computation-run (computation-each (computation-with! (var-x 'temp))
                          (computation-fn ((x var-x)) (computation-pure x))
                        ) ;computation-each
       ) ;computation-run
  =>
  'temp
) ;check

(check (computation-run (computation-each (computation-with! (var-x 'modified))
                          (computation-fn ((x var-x)) (computation-pure x))
                        ) ;computation-each
       ) ;computation-run
  =>
  'modified
) ;check





(define-computation-type make-show-env
  show-run
  (port (current-output-port))
  (col 0)
  (row 0)
  (width 78)
  (radix 10)
  (pad-char #\space)
  (substring/width substring)
  (substring/preserve #f)
  (word-separator? char-whitespace?)
  (ambiguous-is-wide? #f)
  (ellipsis "")
  (decimal-align #f)
  (decimal-sep #f)
  (comma-sep #f)
  (comma-rule #f)
  (sign-rule #f)
  (precision #f)
  (writer #f)
) ;define-computation-type

(show-run (computation-fn ((p port) (w width))
            (check-true (port? p))
            (check w => 78)
            (computation-pure 'done)
          ) ;computation-fn
) ;show-run

(show-run (computation-with ((width 40) (col 10))
            (computation-fn ((w width)) (check w => 40) (computation-pure w))
          ) ;computation-with
) ;show-run
(check (show-run (computation-fn (width) (computation-pure width))) => 78)

(show-run (computation-each (computation-fn ((c col)) (check c => 0) (computation-pure (+ c 1)))
            (computation-fn ((c col)) (check c => 0) (computation-pure 'done))
          ) ;computation-each
) ;show-run


(check-report)
