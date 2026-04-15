(import (liii check) (srfi srfi-165))

(check-set-mode! 'report-failed)




(define test-var-1
  (make-computation-environment-variable 'test-var-1
    "default"
    #f
  ) ;make-computation-environment-variable
) ;define
(define test-var-2
  (make-computation-environment-variable 'test-var-2
    42
    #t
  ) ;make-computation-environment-variable
) ;define
(define test-var-1*
  (make-computation-environment-variable 'test-var-1
    "default"
    #f
  ) ;make-computation-environment-variable
) ;define

(check-false (eq? test-var-1 test-var-1*)
) ;check-false




(define env-1
  (make-computation-environment)
) ;define
(define env-2
  (make-computation-environment)
) ;define

(check-true (vector? env-1))
(check-false (eq? env-1 env-2))

(check (computation-environment-ref env-1
         default-computation
       ) ;computation-environment-ref
  =>
  #f
) ;check




(define var-x
  (make-computation-environment-variable 'x
    100
    #f
  ) ;make-computation-environment-variable
) ;define
(define var-y
  (make-computation-environment-variable 'y
    200
    #f
  ) ;make-computation-environment-variable
) ;define

(check (computation-environment-ref env-1
         var-x
       ) ;computation-environment-ref
  =>
  100
) ;check
(check (computation-environment-ref env-1
         var-y
       ) ;computation-environment-ref
  =>
  200
) ;check

(check-catch 'wrong-type-arg
  (computation-environment-ref 'not-env
    var-x
  ) ;computation-environment-ref
) ;check-catch



(define env-updated
  (computation-environment-update env-1
    var-x
    999
  ) ;computation-environment-update
) ;define

(check (computation-environment-ref env-1
         var-x
       ) ;computation-environment-ref
  =>
  100
) ;check

(check (computation-environment-ref env-updated
         var-x
       ) ;computation-environment-ref
  =>
  999
) ;check

(define env-multi
  (computation-environment-update env-1
    var-x
    1
    var-y
    2
  ) ;computation-environment-update
) ;define
(check (computation-environment-ref env-multi
         var-x
       ) ;computation-environment-ref
  =>
  1
) ;check
(check (computation-environment-ref env-multi
         var-y
       ) ;computation-environment-ref
  =>
  2
) ;check

(check (let ((global-env (begin
                           (computation-environment-update! env-1
                             var-x
                             'global
                           ) ;computation-environment-update!
                           env-1
                         ) ;begin
             ) ;global-env
            ) ;
         (computation-environment-ref (computation-environment-update global-env
                                        var-x
                                        'local
                                      ) ;computation-environment-update
           var-x
         ) ;computation-environment-ref
       ) ;let
  =>
  'local
) ;check




(define env-mutable
  (make-computation-environment)
) ;define

(computation-environment-update! env-mutable
  var-x
  'modified
) ;computation-environment-update!
(check (computation-environment-ref env-mutable
         var-x
       ) ;computation-environment-ref
  =>
  'modified
) ;check

(define dynamic-var
  (make-computation-environment-variable 'dynamic
    'initial
    #f
  ) ;make-computation-environment-variable
) ;define
(computation-environment-update! env-mutable
  dynamic-var
  'new-value
) ;computation-environment-update!
(check (computation-environment-ref env-mutable
         dynamic-var
       ) ;computation-environment-ref
  =>
  'new-value
) ;check

(computation-environment-update! env-mutable
  var-x
  'first
) ;computation-environment-update!
(computation-environment-update! env-mutable
  var-x
  'second
) ;computation-environment-update!
(check (computation-environment-ref env-mutable
         var-x
       ) ;computation-environment-ref
  =>
  'second
) ;check





(check (computation-run (make-computation (lambda (compute)
                                            (compute (computation-pure 42))
                                          ) ;lambda
                        ) ;make-computation
       ) ;computation-run
  =>
  42
) ;check

(let ((c (make-computation (lambda (compute)
                             (compute (computation-pure 'first))
                           ) ;lambda
         ) ;make-computation
      ) ;c
     ) ;
  (check (computation-run c) => 'first)
  (check (computation-run c) => 'first)
) ;let

(check (let ((test-var (make-computation-environment-variable 'test
                         100
                         #f
                       ) ;make-computation-environment-variable
             ) ;test-var
            ) ;
         (computation-run (computation-with ((test-var 999))
                            (make-computation (lambda (compute)
                                                (compute (computation-fn ((v test-var))
                                                           (computation-pure v)
                                                         ) ;computation-fn
                                                ) ;compute
                                              ) ;lambda
                            ) ;make-computation
                          ) ;computation-with
         ) ;computation-run
       ) ;let
  =>
  999
) ;check

(check (computation-run (make-computation (lambda (compute)
                                            (compute (make-computation (lambda (compute2)
                                                                         (compute2 (computation-pure 'nested))
                                                                       ) ;lambda
                                                     ) ;make-computation
                                            ) ;compute
                                          ) ;lambda
                        ) ;make-computation
       ) ;computation-run
  =>
  'nested
) ;check

(check (computation-run (computation-bind (make-computation (lambda (compute)
                                                              (compute (computation-pure 10))
                                                            ) ;lambda
                                          ) ;make-computation
                          (lambda (x) (computation-pure (* x 2)))
                        ) ;computation-bind
       ) ;computation-run
  =>
  20
) ;check





(check (computation-run (computation-pure 'test)
       ) ;computation-run
  =>
  'test
) ;check

(check (let ((counter (make-computation-environment-variable 'counter
                        0
                        #f
                      ) ;make-computation-environment-variable
             ) ;counter
            ) ;
         (computation-run (computation-bind (computation-with! (counter 1))
                            (lambda (_)
                              (computation-fn ((c counter))
                                (computation-pure c)
                              ) ;computation-fn
                            ) ;lambda
                          ) ;computation-bind
         ) ;computation-run
       ) ;let
  =>
  1
) ;check

(check (let ((counter (make-computation-environment-variable 'counter
                        0
                        #f
                      ) ;make-computation-environment-variable
             ) ;counter
            ) ;
         (computation-run (computation-each (computation-with! (counter 2))
                            (computation-fn ((c counter))
                              (computation-pure c)
                            ) ;computation-fn
                          ) ;computation-each
         ) ;computation-run
       ) ;let
  =>
  2
) ;check

(check (computation-run (computation-fn ((c (make-computation-environment-variable 'counter
                                              0
                                              #f
                                            ) ;make-computation-environment-variable
                                         ) ;c
                                        ) ;
                          (computation-pure c)
                        ) ;computation-fn
       ) ;computation-run
  =>
  0
) ;check




(define var-ask
  (make-computation-environment-variable 'ask
    42
    #f
  ) ;make-computation-environment-variable
) ;define

(check (computation-run (computation-bind (computation-ask)
                          (lambda (env)
                            (computation-pure (computation-environment-ref env
                                                var-ask
                                              ) ;computation-environment-ref
                            ) ;computation-pure
                          ) ;lambda
                        ) ;computation-bind
       ) ;computation-run
  =>
  42
) ;check




(define var-local
  (make-computation-environment-variable 'local
    'global-val
    #f
  ) ;make-computation-environment-variable
) ;define

(check (computation-run (computation-local (lambda (env)
                                             (computation-environment-update env
                                               var-x
                                               'local-val
                                             ) ;computation-environment-update
                                           ) ;lambda
                          (computation-bind (computation-ask)
                            (lambda (e)
                              (computation-pure (computation-environment-ref e var-x)
                              ) ;computation-pure
                            ) ;lambda
                          ) ;computation-bind
                        ) ;computation-local
       ) ;computation-run
  =>
  'local-val
) ;check

(check (computation-environment-ref env-1
         var-local
       ) ;computation-environment-ref
  =>
  'global-val
) ;check



(check (computation-run (computation-pure 42))
  =>
  42
) ;check
(check (computation-run (computation-pure 'hello)
       ) ;computation-run
  =>
  'hello
) ;check

(check (call-with-values (lambda ()
                           (computation-run (computation-pure 1 2 3)
                           ) ;computation-run
                         ) ;lambda
         list
       ) ;call-with-values
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
         (computation-run (computation-each (make-computation (lambda (k)
                                                                (set! result (cons 1 result))
                                                                (k (computation-pure 'void))
                                                              ) ;lambda
                                            ) ;make-computation
                            (make-computation (lambda (k)
                                                (set! result (cons 2 result))
                                                (k (computation-pure 'void))
                                              ) ;lambda
                            ) ;make-computation
                            (make-computation (lambda (k)
                                                (set! result (cons 3 result))
                                                (k (computation-pure 'void))
                                              ) ;lambda
                            ) ;make-computation
                          ) ;computation-each
         ) ;computation-run
         result
       ) ;let
  =>
  '(3 2 1)
) ;check




(check (computation-run (computation-each-in-list (list (computation-pure 'a)
                                                    (computation-pure 'b)
                                                    (computation-pure 'c)
                                                  ) ;list
                        ) ;computation-each-in-list
       ) ;computation-run
  =>
  'c
) ;check



(check (computation-run (computation-bind (computation-pure 5)
                          (lambda (x) (computation-pure (* x 2)))
                        ) ;computation-bind
       ) ;computation-run
  =>
  10
) ;check

(check (computation-run (computation-bind (computation-pure 1 2)
                          (lambda (a b)
                            (computation-pure (+ a b))
                          ) ;lambda
                        ) ;computation-bind
       ) ;computation-run
  =>
  3
) ;check

(check (computation-run (computation-bind (computation-pure 10)
                          (lambda (x)
                            (computation-bind (computation-pure 20)
                              (lambda (y) (computation-pure (+ x y)))
                            ) ;computation-bind
                          ) ;lambda
                        ) ;computation-bind
       ) ;computation-run
  =>
  30
) ;check




(check (computation-run (computation-sequence (list (computation-pure 1)
                                                (computation-pure 2)
                                                (computation-pure 3)
                                              ) ;list
                        ) ;computation-sequence
       ) ;computation-run
  =>
  '(1 2 3)
) ;check

(check (computation-run (computation-sequence '())
       ) ;computation-run
  =>
  '()
) ;check

(check (computation-run (computation-sequence (list (computation-pure 'a)
                                                (computation-pure 'b)
                                              ) ;list
                        ) ;computation-sequence
       ) ;computation-run
  =>
  '(a b)
) ;check

(check (let ((n 0))
         (computation-run (computation-sequence (list (make-computation (lambda (k)
                                                                          (set! n (+ n 1))
                                                                          (k (computation-pure n))
                                                                        ) ;lambda
                                                      ) ;make-computation
                                                  (make-computation (lambda (k)
                                                                      (set! n (+ n 1))
                                                                      (k (computation-pure n))
                                                                    ) ;lambda
                                                  ) ;make-computation
                                                  (make-computation (lambda (k)
                                                                      (set! n (+ n 1))
                                                                      (k (computation-pure n))
                                                                    ) ;lambda
                                                  ) ;make-computation
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
                            (computation-fn ((x var-x))
                              (computation-pure x)
                            ) ;computation-fn
                          ) ;computation-forked
                        ) ;computation-with
       ) ;computation-run
  =>
  'shared
) ;check

(check (computation-run (computation-with ((var-x 0))
                          (computation-forked (computation-with! (var-x 999))
                            (computation-pure 'done)
                          ) ;computation-forked
                          (computation-fn ((x var-x))
                            (computation-pure x)
                          ) ;computation-fn
                        ) ;computation-with
       ) ;computation-run
  =>
  0
) ;check




(check (computation-run (computation-with ((var-x 'original))
                          (computation-bind/forked (computation-with! (var-x 'changed))
                            (lambda (_)
                              (computation-fn ((x var-x))
                                (computation-pure x)
                              ) ;computation-fn
                            ) ;lambda
                          ) ;computation-bind/forked
                        ) ;computation-with
       ) ;computation-run
  =>
  'original
) ;check




(check (computation-run (computation-fn ((x var-x))
                          (computation-pure x)
                        ) ;computation-fn
       ) ;computation-run
  =>
  100
) ;check

(check (computation-run (computation-fn (var-x)
                          (computation-pure var-x)
                        ) ;computation-fn
       ) ;computation-run
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
                          (computation-fn ((x var-x))
                            (computation-pure x)
                          ) ;computation-fn
                        ) ;computation-with
       ) ;computation-run
  =>
  999
) ;check

(check (computation-run (computation-with ((var-x 1) (var-y 2))
                          (computation-each (computation-pure 'ignored)
                          ) ;computation-each
                          (computation-fn ((x var-x) (y var-y))
                            (computation-pure (list x y))
                          ) ;computation-fn
                        ) ;computation-with
       ) ;computation-run
  =>
  '(1 2)
) ;check

(check (computation-run (computation-with ((var-x 'outer))
                          (computation-with ((var-x 'inner))
                            (computation-fn ((x var-x))
                              (computation-pure x)
                            ) ;computation-fn
                          ) ;computation-with
                        ) ;computation-with
       ) ;computation-run
  =>
  'inner
) ;check




(check (computation-run (computation-each (computation-with! (var-x 'temp))
                          (computation-fn ((x var-x))
                            (computation-pure x)
                          ) ;computation-fn
                        ) ;computation-each
       ) ;computation-run
  =>
  'temp
) ;check

(check (computation-run (computation-each (computation-with! (var-x 'modified))
                          (computation-fn ((x var-x))
                            (computation-pure x)
                          ) ;computation-fn
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
            (computation-fn ((w width))
              (check w => 40)
              (computation-pure w)
            ) ;computation-fn
          ) ;computation-with
) ;show-run
(check (show-run (computation-fn (width)
                   (computation-pure width)
                 ) ;computation-fn
       ) ;show-run
  =>
  78
) ;check

(show-run (computation-each (computation-fn ((c col))
                              (check c => 0)
                              (computation-pure (+ c 1))
                            ) ;computation-fn
            (computation-fn ((c col))
              (check c => 0)
              (computation-pure 'done)
            ) ;computation-fn
          ) ;computation-each
) ;show-run


(check-report)
