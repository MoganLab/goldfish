(import (liii check) (liii hash-table) (liii packrat))


(check-set-mode! 'report-failed)


(define (generator tokens)
  (let ((stream tokens))
    (lambda ()
      (if (null? stream)
        (values #f #f)
        (let ((base-token (car stream)))
          (set! stream (cdr stream))
          (values #f base-token)
        ) ;let
      ) ;if
    ) ;lambda
  ) ;let
) ;define


;; simple parser


(define simple-parser
  (packrat-parser expr (expr ((a <- 'num) a) ((a <- 'id) a)))
) ;define
(check-true (procedure? simple-parser))


(let* ((gen-num (generator '((num . 123))))
       (r-num (simple-parser (base-generator->results gen-num)))
      ) ;
  (check-true (parse-result-successful? r-num))
  (check (parse-result-semantic-value r-num) => 123)
) ;let*


(let* ((gen-id (generator '((id . foo))))
       (r-id (simple-parser (base-generator->results gen-id)))
      ) ;
  (check-true (parse-result-successful? r-id))
  (check (parse-result-semantic-value r-id) => 'foo)
) ;let*


(let* ((gen-invalid (generator '((foo . bar))))
       (r-invalid (simple-parser (base-generator->results gen-invalid)))
      ) ;
  (check-false (parse-result-successful? r-invalid))
) ;let*


;; calc


(define calc-env (make-hash-table))
(define calc
  (packrat-parser expr
    (expr (('begin body <- exprs 'end) body)
     ((var <- 'id ':= val <- expr) (hash-table-set! calc-env var val))
     ((a <- mulexp '+ b <- expr) (+ a b))
     ((a <- mulexp '- b <- expr) (- a b))
     ((a <- mulexp) a)
    ) ;expr
    (mulexp ((a <- powexp '* b <- mulexp) (* a b))
     ((a <- powexp '/ b <- mulexp) (/ a b))
     ((a <- powexp) a)
    ) ;mulexp
    (powexp ((a <- simple '^ b <- powexp) (expt a b)) ((a <- simple) a))
    (simple ((a <- 'num) a)
     ((a <- 'id) (calc-env a))
     (('oparen a <- expr 'cparen) a)
    ) ;simple
    (exprs ((a <- expr rest <- exprs) rest) ((a <- expr) a))
  ) ;packrat-parser
) ;define
(check-true (procedure? calc))


(let* ((g (generator '((num . 2) (+) (num . 3))))
       (expected (+ 2 3))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)


;; NOTE: the `calc` parser is right recursion;
;;       packrat hates left recursion
(let* ((g (generator '((num . 1) (-) (num . 2) (+) (num . 3))))
       (expected (- 1 (+ 2 3)))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)


;; ditto
(let* ((g (generator '((num . 1) (*) (num . 2) (/) (num . 3))))
       (expected (* 1 (/ 2 3)))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)


(let* ((g (generator '((oparen) (num . 2) (+) (num . 3) (cparen) (*) (num . 4))))
       (expected (* (+ 2 3) 4))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)


(let* ((g (generator '((num . 2) (^) (num . 3))))
       (expected (expt 2 3))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)


(let* ((g (generator '((num . 8) (/) (num . 2))))
       (expected (/ 8 2))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)


(let* ((g (generator '((begin)
                       (id . ans)
                       (:=)
                       (num . 42)
                       (oparen)
                       (num . 2)
                       (+)
                       (id . ans)
                       (cparen)
                       (^)
                       (num . 3)
                       (end))
          ) ;generator
       ) ;g
       (expected (begin (define ans 42) (expt (+ 2 ans) 3)))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)


(let* ((g (generator '((oparen)
                       (num . 2)
                       (+)
                       (num . 3)
                       (cparen)
                       (^)
                       (oparen)
                       (num . 1)
                       (+)
                       (num . 1)
                       (cparen))
          ) ;generator
       ) ;g
       (expected (expt (+ 2 3) (+ 1 1)))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)


(let* ((g (generator '((begin)
                       (id . a)
                       (:=)
                       (num . 10)
                       (id . b)
                       (:=)
                       (num . 20)
                       (id . a)
                       (*)
                       (id . b)
                       (end))
          ) ;generator
       ) ;g
       (expected (begin (define a 10) (define b 20) (* a b)))
       (r (calc (base-generator->results g)))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)


(let* ((g-invalid (generator '((begin) (foo . bar) (end))))
       (r-invalid (calc (base-generator->results g-invalid)))
      ) ;
  (check-false (parse-result-successful? r-invalid))
) ;let*
(hash-table-clear! calc-env)




(let ()
  (define success (make-result 42 #f))
  (check-true (parse-result? success))
) ;let




(let ()
  (check-true (parse-result? (make-result 42 #f)))
  (check-false (parse-result? 42))
) ;let




(let ()
  (define success (make-result 42 #f))
  (check-true (parse-result-successful? success))
) ;let




(let ()
  (define success (make-result 42 #f))
  (check (parse-result-semantic-value success) => 42)
) ;let




(let ()
  (define fail (make-expected-result (make-parse-position #f 1 0) "num"))
  (check-false (parse-result-successful? fail))
) ;let




(let ()
  (define pos (make-parse-position "test.scm" 1 5))
  (define message (make-message-result pos "error"))
  (check-false (parse-result-successful? message))
) ;let




(let ()
  (define success (make-result 42 #f))
  (check (parse-result-next success) => #f)
) ;let




(let ()
  (define pos (make-parse-position "test.scm" 3 15))
  (check-true (parse-position? pos))
) ;let




(let ()
  (define pos (make-parse-position "test.scm" 3 15))
  (check-true (parse-position? pos))
) ;let




(let ()
  (define pos (make-parse-position "test.scm" 3 15))
  (check (parse-position-file pos) => "test.scm")
) ;let




(let ()
  (define pos (make-parse-position "test.scm" 3 15))
  (check (parse-position-line pos) => 3)
) ;let




(let ()
  (define pos (make-parse-position "test.scm" 3 15))
  (check (parse-position-column pos) => 15)
) ;let




(let ()
  (define gen (lambda () (values (make-parse-position "test" 1 0) #f)))
  (define results (base-generator->results gen))
  (check-true (parse-results? results))
) ;let




(let ()
  (define gen
    (let ((tokens '((num . 100) (id . x))))
      (lambda ()
        (if (null? tokens)
          (values #f #f)
          (let ((token (car tokens)))
            (set! tokens (cdr tokens))
            (values #f token)
          ) ;let
        ) ;if
      ) ;lambda
    ) ;let
  ) ;define
  (define results (base-generator->results gen))
  (check (parse-results-token-kind results) => 'num)
) ;let




(let ()
  (define gen
    (let ((tokens '((num . 100))))
      (lambda ()
        (if (null? tokens)
          (values #f #f)
          (let ((token (car tokens)))
            (set! tokens (cdr tokens))
            (values #f token)
          ) ;let
        ) ;if
      ) ;lambda
    ) ;let
  ) ;define
  (define results (base-generator->results gen))
  (check (parse-results-token-kind results) => 'num)
) ;let




(let ()
  (define gen
    (let ((tokens '((num . 100))))
      (lambda ()
        (if (null? tokens)
          (values #f #f)
          (let ((token (car tokens)))
            (set! tokens (cdr tokens))
            (values #f token)
          ) ;let
        ) ;if
      ) ;lambda
    ) ;let
  ) ;define
  (define results (base-generator->results gen))
  (check (parse-results-token-value results) => 100)
) ;let




(let ()
  (define pos (make-parse-position "test.scm" 2 10))
  (define error-ex (make-error-expected pos "open-paren"))
  (check-true (parse-error? error-ex))
) ;let




(let ()
  (define pos (make-parse-position "test.scm" 2 10))
  (define error-msg (make-error-message pos "syntax error"))
  (check-true (parse-error? error-msg))
) ;let




(let ()
  (define pos (make-parse-position "test.scm" 2 10))
  (check-true (parse-error? (make-error-expected pos "test")))
) ;let




(let ()
  (define pos (make-parse-position "test.scm" 2 10))
  (define error-ex (make-error-expected pos "open-paren"))
  (check (parse-error-position error-ex) => pos)
) ;let




(let ((gen (generator '((num . 42)))))
  (define %parse-num
    (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r))))
  ) ;define
  (define result (%parse-num (base-generator->results gen)))
  (check-true (parse-result-successful? result))
  (check (parse-result-semantic-value result) => 42)
) ;let




(let* ((gen (generator '((num . 777))))
       (%parse-num (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r))))
       ) ;%parse-num
       (%parse-id (packrat-check-base 'id (lambda (v) (lambda (r) (make-result v r)))))
       (%parse-or (packrat-or %parse-num %parse-id))
      ) ;
  (let ((r (%parse-or (base-generator->results gen))))
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => 777)
  ) ;let
) ;let*




(let ((gen (generator '((num . 25)))))
  (define %parse-num
    (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r))))
  ) ;define
  (define %parse-check
    (packrat-check %parse-num (lambda (n) (lambda (r) (make-result (* n 2) r))))
  ) ;define
  (define result (%parse-check (base-generator->results gen)))
  (check-true (parse-result-successful? result))
  (check (parse-result-semantic-value result) => 50)
) ;let




(let ((gen-id (generator '((id . test)))))
  (define %parse-num
    (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r))))
  ) ;define
  (define %parse-id
    (packrat-check-base 'id (lambda (v) (lambda (r) (make-result v r))))
  ) ;define
  (define %parse-unless (packrat-unless "not expected" %parse-num %parse-id))
  (let ((r (%parse-unless (base-generator->results gen-id))))
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => 'test)
  ) ;let
) ;let




(let ()
  (define calc
    (packrat-parser expr
      (expr ((a <- mulexp '+ b <- expr) (+ a b)) ((a <- mulexp) a))
      (mulexp ((a <- simple '* b <- mulexp) (* a b)) ((a <- simple) a))
      (simple ((a <- 'num) a) (('oparen a <- expr 'cparen) a))
    ) ;packrat-parser
  ) ;define

  (let* ((g (generator '((num . 2) (+) (num . 3))))
         (expected (+ 2 3))
         (r (calc (base-generator->results g)))
        ) ;
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => expected)
  ) ;let*
) ;let


(check-report)
