(library (extensible-match-test)
  (export run-tests)
  (import (rnrs (6))
          (rnrs eval (6))
          (only (srfi :1 lists) iota lset=)
          (srfi :6 basic-string-ports)
          (srfi :39 parameters)
          (extensible-match)
          (chibi test))

  (define (run-tests)
    (define test-env (environment '(rnrs (6)) '(extensible-match)))
    (test-group "Basic functionality"
      (test "Matches anything" #t
        (match 'x
          (_ #t)))

      (test-error "Failure to match signals an error" match-violation?
        (match 'x
          ('y #f)))

      (test "Matches and binds" 'x
        (match 'x
          (x x)
          (_ 'fail)))

      (test "Matches a literal number" #t
        (match 42
          (42 #t)
          (_ #f)))

      (test "Matches a literal string" #t
        (match "hello"
          ("hello" #t)
          (_ #f)))

      (test "Matches a quoted symbol" #t
        (match 'x
          ('x #t)
          (_ #f)))

      (test "Matches quoted null" #t
        (match '()
          ('() #t)
          (_ #f)))

      (test-error "Unquoted null is a syntax error" syntax-violation?
        (eval
         '(match '()
            (() #f)
            (_ #f))
         test-env))

      (test "Leftmost matching clause wins" 'x
        (match 'x
          (v v)
          (_ #t))))

    (test-group "Primitive patterns"
      (test "? matches when true" #t
        (match 42
          ((? even?) #t)
          (_ #f)))
      (test "? doesn’t match when false" #f
        (match 21
          ((? even?) #t)
          (_ #f)))

      (test "=> matches one subpattern" #t
        (match 'x
          ((=> symbol->string "x") #t)
          (_ #f)))
      (test "=> doesn’t match when subpattern doesn’t match" #f
        (match 'y
          ((=> symbol->string "x") #t)
          (_ #f)))

      (test "=> matches multiple values against multiple subpatterns" #t
        (match 5
          ((=> (lambda (x) (div-and-mod x 3)) 1 2) #t)
          (_ #f)))
      (test "=> doesn’t match when one subpattern doesn’t match its value" #f
        (match 5
          ((=> (lambda (x) (div-and-mod x 3)) _ 0) #t)
          (_ #f)))

      (test "=> binds variables" 'x
        (match '(x)
          ((=> car v) v)
          (_ 'fail)))
      (test "=> binds multiple values" '(1 . 2)
        (match 5
          ((=> (lambda (x) (div-and-mod x 3)) x y)
           (cons x y))
          (_ 'fail)))

      (test "and matches when all subpatterns match" #t
        (match 42
          ((and (? exact?) (? even?)) #t)
          (_ #f)))
      (test "and doesn’t match when one subpattern doesn’t match" #f
        (match 42
          ((and (? exact?) (? odd?)) #t)
          (_ #f)))

      (test "empty and matches anything" #t
        (match 'a
          ((and) #t)
          (_ #f)))
      (test "one-armed and matches its subpattern" #t
        (match 'b
          ((and 'b) #t)
          (_ #f)))

      (test "and matches left-to-right and short-circuits" 'not-a-number
        (match 'x
          ((and (? number?) (=> inexact x)) x)
          (_ 'not-a-number)))

      (test "and binds pattern variables" '(1 . 1)
        (match 1
          ((and (? number?) x y) (cons x y))
          (_ 'fail)))

      (test "or matches when one of its subpatterns matches" #t
        (match 42
          ((or 23 42) #t)
          (_ #f)))
      (test "or doesn’t match when none of its subpatterns match" #f
        (match 42
          ((or 23 144) #t)
          (_ #f)))

      (test "empty or matches nothing" #f
        (match 'a
          ((or) #t)
          (_ #f)))
      (test "one-armed or matches its subpattern" #t
        (match 'b
          ((or 'b) #t)
          (_ #f)))

      (test "or matches left-to-right and short-circuits" #t
        (let ((atom? (lambda (x) (not (pair? x)))))
          (match 'x
            ((or (? atom?) (=> car _)) #t)
            (_ #f))))

      (test "or binds pattern variables" 42
        (match 42
          ((or (and x 42) (and x 144)) x)
          (_ 'fail)))
      (test "No syntax error binding disjointed variables in ‘or’" #t
        (eval
         '(match 'x
            ((or x (and x y)) #t)
            (_ #f))
         test-env))
      (test-error "Syntax error to reference a disjointed variable in ‘or’" syntax-violation?
        (eval
         '(let ((y 1))
            (match '(a)
              ((or (cons x '()) (cons x y))
               (if (symbol? x) x y))))
         test-env))
      (test-error "Syntax error to reference a matched disjointed variable in ‘or’" syntax-violation?
        (eval
         '(let ((y 1))
            (match '(a . 5)
              ((or (cons x '()) (cons x y))
               (if (symbol? x) x y))))
         test-env))

      (test "not doesn’t match when its subpattern matches" #f
        (match 42
          ((not 42) #t)
          (_ #f)))
      (test "not matches when its subpattern doesn’t match" #t
        (match 144
          ((not 42) #t)
          (_ #f)))

      (test "not suppresses pattern variables" 'outer
        (let ((var 'outer))
          (match 'inner
            ((not (and (? number?) var)) var)
            (_ 'fail))))
      (test "not not suppresses pattern variables" 'outer
        (let ((var 'outer))
          (match 5
            ((not (not (and (? number?) var))) var)
            (_ 'fail))))

      (test "not composes with and, matches" #f
        (match 'four
          ((not (and (? number?) 4)) #f)
          (_ #t)))
      (test "not composes with and, doesn’t match" #t
        (match 4
          ((not (and (? number?) 4)) #f)
          (_ #t)))
      (test "not composes with or, matches" #f
        (match 4
          ((not (or 1 2 3)) #f)
          (_ #t)))
      (test "not composes with or, doesn’t match" #t
        (match 3
          ((not (or 1 2 3)) #f)
          (_ #t))))

    (test-group "Non-linear pattern detection"
      (test-error "Non-linear pattern with match-values" syntax-violation?
        (eval
         '(match-values (values 1 1)
            ((x x) #t)
            ((_ _) #f))
         test-env))
      (test-error "Basic non-linear pattern with and" syntax-violation?
        (eval
         '(match 1
            ((and x x) #t)
            (_ #f))
         test-env))
      (test-error "Non-linear pattern local to one or clause" syntax-violation?
        (eval
         '(match '(1 . 1)
            ((or (cons x x) x) #t)
            (_ #f))
         test-env))
      (test-error "Non-linear pattern use in not, after first occurrence" syntax-violation?
        (eval
         '(match 1
            ((and x (not x)) #t)
            (_ #f))
         test-env))
      (test-error "Non-linear pattern use in not, before first occurrence" syntax-violation?
        (eval
         '(match 1
            ((and (not x) x) #t)
            (_ #f))
         test-env)))

    (test-group "Basic derived patterns"
      (test "cons pattern matches a pair" '(2 . 1)
        (match '(1 . 2)
          ((cons x y)
           (cons y x))
          (_ 'fail)))
      (test "cons pattern doesn’t match a non-pair" 'null
        (match '()
          ((cons x y) 'pair)
          ('() 'null)
          (_ 'fail))))

    (test-group "More advanced combinations of patterns"
      ;; The challenge with most of these, especially the later ones,
      ;; is not in the directly observable behaviour but in how well
      ;; the implementation can optimize the resulting tree of
      ;; conditionals and bindings.
      (let ()
        (define (quadrant pt)
          (match pt
            ((cons (? positive?) (? positive?)) 1)
            ((cons (? negative?) (? positive?)) 2)
            ((cons (? negative?) (? negative?)) 3)
            ((cons (? positive?) (? negative?)) 4)))
        (test "No variable bindings 1" 1 (quadrant '(5 . 5)))
        (test "No variable bindings 2" 2 (quadrant '(-5 . 5)))
        (test "No variable bindings 3" 3 (quadrant '(-5 . -5)))
        (test "No variable bindings 4" 4 (quadrant '(5 . -5)))
        (test-error "No variable bindings nothing matches" match-violation?
          (quadrant '(0 . 1))))

      (test-values "or binds pattern variables with different structures" (values '(1 2 3) '(4 5 6))
        (values
         (match '((1 . 2) . 3)
           ((or (cons (cons a b) c) (cons a (cons b c))) (list a b c)))
         (match '(4 . (5 . 6))
           ((or (cons (cons a b) c) (cons a (cons b c))) (list a b c)))))

      (test "cons patterns distinguished by refutable car, first case" 1
        (match '(a . 1)
          ((cons 'a x) x)
          ((cons 'b x) 'fail)
          (_ #f)))
      (test "cons patterns distinguished by refutable car, second case" 2
        (match '(b . 2)
          ((cons 'a x) 'fail)
          ((cons 'b x) x)
          (_ #f)))
      (test "cons patterns distinguished by refutable cdr, first case" 1
        (match '(1)
          ((cons a '()) a)
          ((cons a _) (list 'improper a))
          (_ #f)))
      (test "cons patterns distinguished by refutable cdr, second case" '(improper 1)
        (match '(1 . 2)
          ((cons a '()) a)
          ((cons a _) (list 'improper a))
          (_ #f)))

      (test "and over or, first case" '(1 . 2)
        (match '(1 . 2)
          ((and (or (? pair?) (? vector?)) x) x)))
      (test "and over or, second case" '#(1 2)
        (match '#(1 2)
          ((and (or (? pair?) (? vector?)) x) x)))

      (let ((maranget-true-false
             (match-lambda
               ((_  #f #t) 1)
               ((#f #t _ ) 2)
               ((_  _  #f) 3)
               ((_  _  #t) 4))))
        (test "Maranget’s true-false example, case 1/T" 1
          (maranget-true-false #t #f #t))
        (test "Maranget’s true-false example, case 1/F" 1
          (maranget-true-false #f #f #t))
        (test "Maranget’s true-false example, case 2/T" 2
          (maranget-true-false #f #t #t))
        (test "Maranget’s true-false example, case 2/F" 2
          (maranget-true-false #f #t #f))
        (test "Maranget’s true-false example, case 3/TF" 3
          (maranget-true-false #t #f #f))
        (test "Maranget’s true-false example, case 3/TT" 3
          (maranget-true-false #t #t #f))
        (test "Maranget’s true-false example, case 3/FF" 3
          (maranget-true-false #f #f #f))
        (test "Maranget’s true-false example, case 4" 4
          (maranget-true-false #t #t #t)))

      (let ((merge/zip
             (match-lambda
               (('() ys) ys)
               ((xs '()) xs)
               (((cons x xs) (cons y ys)) (list 'merge x xs y ys)))))
        (test "List merge/zip, ys is empty" '(1)
          (merge/zip '(1) '()))
        (test "List merge/zip, xs is empty" '(2 3)
          (merge/zip '() '(2 3)))
        (test "List merge/zip, neither is empty" '(merge 1 () 2 (3))
          (merge/zip '(1) '(2 3))))

      (let ((unwieldy
             (match-lambda
               (('() '()) 'a)
               ((xs ys) (values xs ys)))))
        (test-values "Petterson’s unwieldy example, case A" 'a
          (unwieldy '() '()))
        (test-values "Petterson’s unwieldy example, case B" (values '(1) '(2))
          (unwieldy '(1) '(2))))

      (let ()
        (define (last ls)
          ;; Here the challenge is that the implementation (after all
          ;; further optimization by the Scheme compiler) should not
          ;; generate a call to ‘car’ for every iteration through the
          ;; loop, since it’s only needed on the last iteration.
          (match ls
            ((cons elt '()) elt)
            ((cons _ ls*) (last ls*))))
        (test "last procedure" 'z
          (last '(x y z)))
        (test-error "last procedure with empty list"  match-violation?
          (last '())))

      (let ()
        ;; example due to D. Guthrie
        (define (make-incl? A B)
          (lambda (c)
            ;; c >= A, c <= B
            (and (fixnum? c)
                 (fx<=? A c B))))

        (define (utf8-3-sequence seq)
          (match seq
            ((cons* #xE0
                    (? (make-incl? #xA0 #xBF) curr2)
                    (? (make-incl? #x80 #xBF) tail1)
                    rest)
             (values (list #xE0 curr2 tail1)
                     rest))
            ((cons* (? (make-incl? #xE1 #xEC) curr1)
                    (? (make-incl? #x80 #xBF) tail1)
                    (? (make-incl? #x80 #xBF) tail2)
                    rest)
             (values (list curr1 tail1 tail2)
                     rest))))

        (test-values "cons* over ? with a non-identifier procedure expression, case 1"
            (values '(#xE0 #xA1 #xB3) '(#x65))
          (utf8-3-sequence '(#xE0 #xA1 #xB3 #x65)))
        (test-values "cons* over ? with a non-identifier procedure expression, case 2"
            (values '(#xE8 #x85 #x99) '(#x20))
          (utf8-3-sequence '(#xE8 #x85 #x99 #x20)))))

    (test-group "Advanced not patterns"
      (test "Not over cons / pair" #t
        (match '(#f . #f)
          ((not (cons _ '())) #t)
          ((cons x '()) x)))

      (test "Not over cons / vector" #t
        (match '#(1 2)
          ((not (cons _ '())) #t)
          ((cons x '()) x)))

      (let ()
        (define-record-type a)
        (define-record-type b (fields x) (parent a))
        (test "Match with supertype" #t
          (match (make-b 1)
            ((not (? b? (=> b-x 1))) #f)
            ((? a?) #t)))
        (test "Match with supertype not" #t
          (match (make-b 1)
            ((not (? a?)) #f)
            ((? b? (? b? (=> b-x 1))) #t)))
        (test "Match with subtype" #t
          (match (make-a)
            ((not (? b? (=> b-x 1))) #t)
            ((? a?) #f)))))

    (test-group "Sequence patterns"
      (define (mkseq . xs) (list->vector xs))
      (define (len xs) (vector-length xs))
      (define (ref xs idx)
        (vector-ref xs idx))
      (test "Sequence with a fixed length matches" #t
        (match (mkseq 1 2 3)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             1 2 3) #t)
          (_ #f)))
      (test "Sequence pattern with a fixed length doesn’t match underlong input" #f
        (match (mkseq 1 2)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             1 2 3) #t)
          (_ #f)))
      (test "Sequence pattern with a fixed length doesn’t match overlong input" #f
        (match (mkseq 1 2 3 4)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             1 2 3) #t)
          (_ #f)))

      (test "Sequence pattern with non-matching subpattern doesn’t match" #f
        (match (mkseq 1 2 3)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             1 2 2) #t)
          (_ #f)))

      (test "Basic ellipsized sequence matches" 3
        (match (mkseq 'a 'b 'c)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             x ...) (length x))
          (_ 'fail)))

      (test "Ellipsis matches nothing" '()
        (match (mkseq)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             x ...) x)
          (_ 'fail)))

      (test "Ellipsized sequence pattern with a head" '(a . 2)
        (match (mkseq 'a 'b 'c)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             x y ...)
           (cons x (length y)))
          (_ 'fail)))

      (test "Ellipsized sequence pattern with a tail" '(c . 2)
        (match (mkseq 'a 'b 'c)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             x ... y)
           (cons y (length x)))
          (_ 'fail)))

      (test "Ellipsized sequence pattern with fixed number of repetitions" #t
        (match (mkseq 'a 'a 'b)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             'a (... 2) 'b)
           #t)
          (_ #f)))
      (test "Ellipsized sequence pattern with too many repetitions for fixed number" #f
        (match (mkseq 'a 'a 'a 'b)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             'a (... 2) 'b)
           #t)
          (_ #f)))
      (test "Ellipsized sequence pattern with not enough repetitions for fixed number" #f
        (match (mkseq 'a 'b)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             'a (... 2) 'b) #t)
          (_ #f)))

      (test "Ellipsized sequence pattern with minimum number of repetitions" #t
        (match (mkseq 'a 'b)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             'a (... 1 #t) 'b)
           #t)
          (_ #f)))

      (test "Ellipsized sequence pattern, not enough repetitions for minimum" #f
        (match (mkseq 'a 'b)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             'a (... 2 #t) 'b)
           #t)
          (_ #f)))
      (test "Ellipsized sequence pattern, more than minimum" #t
        (match (mkseq 'a 'a 'a 'b)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             'a (... 1 #t) 'b)
           #t)
          (_ #f)))

      (test "Ellipsized sequence pattern, bounded repetition (matches minimum)" #t
        (match (mkseq 'a 'a 'b)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             'a (... 2 4) 'b)
           #t)
          (_ #f)))
      (test "Ellipsized sequence pattern, bounded repetition (matches between)" #t
        (match (mkseq 'a 'a 'a 'b)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             'a (... 2 4) 'b)
           #t)
          (_ #f)))
      (test "Ellipsized sequence pattern, bounded repetition (matches maximum)" #t
        (match (mkseq 'a 'a 'a 'b)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             'a (... 2 4) 'b)
           #t)
          (_ #f)))
      (test "Ellipsized sequence pattern, bounded repetition (too few)" #f
        (match (mkseq 'a 'b)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             'a (... 2 4) 'b) #t)
          (_ #f)))
      (test "Ellipsized sequence pattern, bounded repetition (too many)" #f
        (match (mkseq 'a 'a 'a 'a 'a 'b)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             'a (... 2 4) 'b) #t)
          (_ #f)))

      (test "Ellipsis is greedy with consecutive ellipsis" '((1 2 3) . ())
        (match (mkseq 1 2 3)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             x ... y ...)
           (cons x y))
          (_ 'fail)))
      (test "Ellipsis is greedy when splitting sequence" '((0 x 1) (2))
        (match (mkseq 0 'x 1 'x 2)
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             a ... 'x b ...)
           (list a b))
          (_ 'fail)))

      (test "Ellipsis (zero or more) binds pattern variables" '((a b c) (1 2 3))
        (match (mkseq '(a . 1) '(b . 2) '(c . 3))
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             (cons x y) ...)
           (list x y))
          (_ 'fail)))
      (test "Ellipsis (fixed) binds pattern variables" '((a b) (1 2))
        (match (mkseq '(a . 1) '(b . 2))
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             (cons x y) (... 2))
           (list x y))
          (_ 'fail)))
      (test "Ellipsis (minimum) binds pattern variables" '((a b) (1 2))
        (match (mkseq '(a . 1) '(b . 2))
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             (cons x y) (... 1 #t))
           (list x y))
          (_ 'fail)))
      (test "Ellipsis (bounded) binds pattern variables" '((a b) (1 2))
        (match (mkseq '(a . 1) '(b . 2))
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             (cons x y) (... 1 2))
           (list x y))
          (_ 'fail)))

      (test "Multiple pattern variables per subpattern" '(a b 1 2)
        (match (mkseq '(a . 1) '(b . 2))
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             (cons let num)
             (cons let2 num2))
           (list let let2 num num2))
          (_ 'fail)))

      (test "Multiple pattern variables per subpattern, with ellipsis"
          '(a 1 (b c) (2 3))
        (match (mkseq '(a . 1) '(b . 2) '(c . 3))
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             (cons let num)
             (cons rest-let rest-num) ...)
           (list let num rest-let rest-num))
          (_ 'fail)))

      (test "Multiple pattern variables per subpattern, with bounded ellipsis"
          '(c 3 (a b) (1 2))
        (match (mkseq '(a . 1) '(b . 2) '(c . 3))
          ((seq xs ((idx 0 (+ idx 1)))
                (= idx (len xs))
                (ref xs idx)
             (cons first-lets first-nums) (... 0 2)
             (cons let num))
           (list let num first-lets first-nums))
          (_ 'fail)))

      (test "Sequence name is not bound as pattern variable" 'foo
        (let ((xs 'foo))
          (match 'bar
            ((seq xs ()
                  #t
                  '())
             xs)
            (_ 'fail))))

      (test "State variables are not bound as pattern variables" 'outer
        (let ((st 'outer))
          (match 'x
            ((seq xs ((st 'inner 'next-inner))
                  #t
                  '())
             st))))

      (test "Sequence name is not visible within subpatterns" 'outer
        (let ((x 'outer))
          (match #f
            ((seq x ((st 'inner_1 'inner_2))
                  (eq? st 'inner_2)
                  st
               (=> (lambda (ign) x) res) _ ...)
             res))))

      (test "State variables are not visible within subpatterns" 'outer
        (let ((x 'outer))
          (match #f
            ((seq ign ((x 'inner_1 'inner_2))
                  (eq? x 'inner_2)
                  x
               (=> (lambda (ign) x) res) _ ...)
             res)))))

    #;(test-group "Sequence patterns (cons-like)"
      (define (mkseq . xs) (list->vector xs))
      (define (len xs) (vector-length xs))
      (define (ref xs idx)
        (vector-ref xs idx))
      (test "Full sequence matches" '(1 2 3 4)
        (match (mkseq 1 2 3 4)
          ((seq/partial xs ((idx 0 (+ idx 1)))
                        (= idx (- (len xs) 1))
                        (ref xs idx)
             a b c d)
           (list a b c d))
          (_ 'fail)))

      (test "Prefix of sequence matches when input is overlong" #t
        (match (mkseq 1 2 3 4)
          ((seq/partial xs ((idx 0 (+ idx 1)))
                        (= idx (- (len xs) 1))
                        (ref xs idx)
             (? number?))
           #t)
          (_ 'fail)))
      (test "No match with an underlong sequence" #f
        (match (mkseq 1 2)
          ((seq/partial xs ((idx 0 (+ idx 1)))
                        (= idx (- (len xs) 1))
                        (ref xs idx)
             1 2 3 4)
           #t)
          (_ #f)))

      (test "Prefix of sequence matches when input diverges from pattern" #t
        (match (mkseq 'a 'b 1 2)
          ((seq/partial xs ((idx 0 (+ idx 1)))
                        (= idx (- (len xs) 1))
                        (ref xs idx)
             (? symbol?) ...)
           #t)
          (_ 'fail))))

    (test-group "Sequence patterns (unordered)"
      (define (mkseq . xs) (list->vector xs))
      (define (len xs) (vector-length xs))
      (define (ref xs idx)
        (vector-ref xs idx))
      (define (lset-equal? . xs) (apply lset= equal? xs))
      ;; shamelessly pinched from Rosetta Code:
      (define (perm s)
        (cond ((null? s) '())
	      ((null? (cdr s)) (list s))
	      (else
	       (let splice ((l '()) (m (car s)) (r (cdr s)))
	         (append
	          (map (lambda (x) (cons m x)) (perm (append l r)))
	          (if (null? r) '()
		      (splice (cons m l) (car r) (cdr r))))))))
      (define (written datum)
        (let ((p (open-output-string)))
          (write datum p)
          (get-output-string p)))

      (test "Basic unordered sequence" #t
        (match (mkseq 2 1 3)
          ((seq/unordered xs ((idx 0 (+ idx 1)))
                          (= idx (len xs))
                          (ref xs idx)
             1 2 3)
           #t)
          (_ #f)))

      (test "Empty unordered sequence" #t
        (match (mkseq)
          ((seq/unordered xs ((idx 0 (+ idx 1)))
                          (= idx (len xs))
                          (ref xs idx))
           #t)
          (_ #f)))

      (test "Long sequence with lots of ambiguous patterns" 78
        (match (apply mkseq (iota 13))
          ((seq/unordered xs ((idx 0 (+ idx 1)))
                          (= idx (len xs))
                          (ref xs idx)
             a b c d e f g h i j k l m)
           (+ a b c d e f g h i j k l m))))

      (test "Extraction of remainder values" '(2 3)
        (match (mkseq 2 1 3)
          ((seq/unordered xs ((idx 0 (+ idx 1)))
                          (= idx (len xs))
                          (ref xs idx)
             1 x ...)
           x)
          (_ #f)))

      (test "Non-matching unordered sequence, refutable patterns first" #f
        (match (mkseq 2 1)
          ((seq/unordered xs ((idx 0 (+ idx 1)))
                          (= idx (len xs))
                          (ref xs idx)
             1 2 a)
           a)
          (_ #f)))

      (test "Non-matching unordered sequence, irrefutable patterns first" #f
        (match (mkseq 2 1)
          ((seq/unordered xs ((idx 0 (+ idx 1)))
                          (= idx (len xs))
                          (ref xs idx)
             a 1 2)
           a)
          (_ #f)))

      (test "Non-matching unordered sequence, with remainder pattern" #f
        (match (mkseq 2 1 'x)
          ((seq/unordered xs ((idx 0 (+ idx 1)))
                          (= idx (len xs))
                          (ref xs idx)
             1 2 (and (? number?) a) ...)
           a)
          (_ #f)))

      (for-each
       (lambda (p)
         (test-equal lset-equal? (string-append "1 2 a b/" (written p)) '(3 4)
           (match (apply mkseq p)
             ((seq/unordered xs ((idx 0 (+ idx 1)))
                             (= idx (len xs))
                             (ref xs idx)
                1 2 a b)
              (list a b))
             (_ #f))))
       (perm '(1 2 3 4)))

      (for-each
       (lambda (p)
         (test-equal lset-equal? (string-append "1 a 2 b/" (written p)) '(3 4)
           (match (apply mkseq p)
             ((seq/unordered xs ((idx 0 (+ idx 1)))
                             (= idx (len xs))
                             (ref xs idx)
                1 a 2 b)
              (list a b))
             (_ #f))))
       (perm '(1 2 3 4)))

      (for-each
       (lambda (p)
         (test-equal lset-equal? (string-append "1 2 x .../" (written p)) '(3 4)
           (match (apply mkseq p)
             ((seq/unordered xs ((idx 0 (+ idx 1)))
                             (= idx (len xs))
                             (ref xs idx)
                1 2 x ...)
              x)
             (_ #f))))
       (perm '(1 2 3 4)))

      (for-each
       (lambda (p)
         (test-equal
           (lambda (x y)
             (and (lset-equal? (car x) (car y))
                  (lset-equal? (cadr x) (cadr y))))
           (string-append "number number any any/" (written p))
           '((5 6) (s t))
           (match (apply mkseq p)
             ((seq/unordered xs ((idx 0 (+ idx 1)))
                             (= idx (len xs))
                             (ref xs idx)
                (and (? number?) x)
                (and (? number?) y)
                a b)
              (list (list x y) (list a b)))
             (_ #f))))
       (perm '(s t 5 6)))

      (for-each
       (lambda (p)
         (test (string-append "number symbol number-or-symbol .../" (written p)) #t
           (match (apply mkseq p)
             ((seq/unordered xs ((idx 0 (+ idx 1)))
                             (= idx (len xs))
                             (ref xs idx)
                (? number?)
                (? symbol?)
                (or (? number?) (? symbol?)) ...)
              #t)
             (_ #f))))
       (perm '(s t 5 6)))

      (for-each
       (lambda (p)
         (test (string-append "number string symbol (no match)/" (written p)) #f
           (match (apply mkseq p)
             ((seq/unordered xs ((idx 0 (+ idx 1)))
                             (= idx (len xs))
                             (ref xs idx)
                (? number?) (? string?) (? symbol?))
              #t)
             (_ #f))))
       (perm '(not-a-number 1 2)))

      (for-each
       (lambda (p)
         (test (string-append "number string symbol ... (no match)/" (written p)) #f
           (match (apply mkseq p)
             ((seq/unordered xs ((idx 0 (+ idx 1)))
                             (= idx (len xs))
                             (ref xs idx)
                (? number?) (? string?) (? symbol?) ...)
              #t)
             (_ #f))))
       (perm '("not-a-number" 1 2)))

      (for-each
       (lambda (p)
         (test-assert (string-append "alist: " (written p))
           (match (apply mkseq p)
             ((seq/unordered xs ((idx 0 (+ idx 1)))
                             (= idx (len xs))
                             (ref xs idx)
                (cons (? odd? x)
                      oddval)
                (cons (? even? y)
                      evenval)
                (cons more-keys more-vals) ...)
              (and (or (and (eqv? x 1)
                            (eqv? oddval 'a)
                            (equal? more-keys '(3))
                            (equal? more-vals '(c)))
                       (and (eqv? x 3)
                            (eqv? oddval 'c)
                            (equal? more-keys '(1))
                            (equal? more-vals '(a))))
                   (eqv? y 2)
                   (eqv? evenval 'b)))
             (_ #f))))
       (perm '((1 . a) (2 . b) (3 . c))))

      (test "Sequence name is not bound as pattern variable" 'foo
        (let ((xs 'foo))
          (match 'bar
            ((seq/unordered xs ()
                            #t
                            '())
             xs)
            (_ 'fail))))

      (test "State variables are not bound as pattern variables" 'outer
        (let ((st 'outer))
          (match 'x
            ((seq/unordered xs ((st 'inner 'next-inner))
                            #t
                            '())
             st))))

      (test "Sequence name is not visible within subpatterns" 'outer
        (let ((x 'outer))
          (match #f
            ((seq/unordered x ((st 'inner_1 'inner_2))
                            (eq? st 'inner_2)
                            st
               (=> (lambda (ign) x) res) _ ...)
             res))))

      (test "State variables are not visible within subpatterns" 'outer
        (let ((x 'outer))
          (match #f
            ((seq/unordered ign ((x 'inner_1 'inner_2))
                            (eq? x 'inner_2)
                            x
               (=> (lambda (ign) x) res) _ ...)
             res)))))

    (test-group "List patterns"
      (test "list pattern with a fixed length matches" #t
        (match '(1 2 3)
          ((list 1 2 3) #t)
          (_ #f)))
      (test "list pattern with a fixed length doesn’t match underlong input" #f
        (match '(1 2)
          ((list 1 2 3) #t)
          (_ #f)))
      (test "list pattern with a fixed length doesn’t match overlong input" #f
        (match '(1 2 3 4)
          ((list 1 2 3) #t)
          (_ #f)))

      (test "list pattern is ellipsized" 3
        (match '(a b c)
          ((list x ...)
           (length x))
          (_ 'fail)))
      (test "Ellipsis matches nothing" '()
        (match '()
          ((list x ...) x)
          (_ 'fail)))

      (test "Ellipsized list pattern with a head" '(a . 2)
        (match '(a b c)
          ((list x y ...)
           (cons x (length y)))
          (_ 'fail)))

      (test "Ellipsized list pattern with a tail" '(c . 2)
        (match '(a b c)
          ((list x ... y)
           (cons y (length x)))
          (_ 'fail)))

      (test "Ellipsized list pattern with fixed number of repetitions" #t
        (match '(a a b)
          ((list 'a (... 2) 'b) #t)
          (_ #f)))
      (test "Ellipsized list pattern with too many repetitions for fixed number" #f
        (match '(a a a b)
          ((list 'a (... 2) 'b) #t)
          (_ #f)))
      (test "Ellipsized list pattern with not enough repetitions for fixed number" #f
        (match '(a b)
          ((list 'a (... 2) 'b) #t)
          (_ #f)))

      (test "Ellipsized list pattern with minimum number of repetitions" #t
        (match '(a b)
          ((list 'a (... 1 #t) 'b) #t)
          (_ #f)))

      (test "Ellipsized list pattern, not enough repetitions for minimum" #f
        (match '(a b)
          ((list 'a (... 2 #t) 'b) #t)
          (_ #f)))
      (test "Ellipsized list pattern, more than minimum" #t
        (match '(a a a b)
          ((list 'a (... 1 #t) 'b) #t)
          (_ #f)))

      (test "Ellipsized list pattern, bounded repetition (matches minimum)" #t
        (match '(a a b)
          ((list 'a (... 2 4) 'b) #t)
          (_ #f)))
      (test "Ellipsized list pattern, bounded repetition (matches between)" #t
        (match '(a a a b)
          ((list 'a (... 2 4) 'b) #t)
          (_ #f)))
      (test "Ellipsized list pattern, bounded repetition (matches maximum)" #t
        (match '(a a a a b)
          ((list 'a (... 2 4) 'b) #t)
          (_ #f)))
      (test "Ellipsized list pattern, bounded repetition (too few)" #f
        (match '(a b)
          ((list 'a (... 2 4) 'b) #t)
          (_ #f)))
      (test "Ellipsized list pattern, bounded repetition (too many)" #f
        (match '(a a a a a b)
          ((list 'a (... 2 4) 'b) #t)
          (_ #f)))

      (test "Ellipsis is greedy with consecutive ellipsis" '((1 2 3) . ())
        (match '(1 2 3)
          ((list x ... y ...)
           (cons x y))
          (_ 'fail)))
      (test "Ellipsis is greedy with pair-matching tail" '((1 2 3) . ())
        (match '(1 2 3)
          ((cons* x ... anything)
           (cons x anything))
          (_ 'fail)))
      (test "Ellipsis is greedy when splitting list" '((0 x 1) (2))
        (match '(0 x 1 x 2)
          ((list a ... 'x b ...)
           (list a b))
          (_ 'fail)))
      (test "Ellipsis is greedy when splitting list with pair-matching tail" '((0 2 1) (3))
        (match '(0 2 1 2 3)
          ((cons* x ... (cons 2 y))
           (list x y))
          (_ 'fail)))

      (test "Ellipsis (zero or more) binds pattern variables" '((a b c) (1 2 3))
        (match '((a . 1) (b . 2) (c . 3))
          ((list (cons x y) ...)
           (list x y))
          (_ 'fail)))
      (test "Ellipsis (fixed) binds pattern variables" '((a b) (1 2))
        (match '((a . 1) (b . 2))
          ((list (cons x y) (... 2))
           (list x y))
          (_ 'fail)))
      (test "Ellipsis (minimum) binds pattern variables" '((a b) (1 2))
        (match '((a . 1) (b . 2))
          ((list (cons x y) (... 1 #t))
           (list x y))
          (_ 'fail)))
      (test "Ellipsis (bounded) binds pattern variables" '((a b) (1 2))
        (match '((a . 1) (b . 2))
          ((list (cons x y) (... 1 2))
           (list x y))
          (_ 'fail)))

      (test-error "Ellipsis as first subpattern is a syntax violation" syntax-violation?
        (eval
         '(match '(x y z)
            ((list ... a b c) #t)
            (_ #f))
         test-env))
      (test-error "Extended ellipsis as first subpattern is a syntax violation" syntax-violation?
        (eval
         '(match '(x y z)
            ((list (... 1 2) a b c) #t)
            (_ #f))
         test-env)))

    (test-group "Vector patterns"
      (test "Basic vector pattern" '(1 2 3 4)
        (match '#(1 2 3 4)
          ((vector a b c d)
           (list a b c d))
          (_ 'fail)))
      (test "Vector pattern with ellipsis" '(1 2 3 4)
        (match '#(1 2 3 4)
          ((vector a ...) a)
          (_ 'fail))))

    (test-group "Quasipatterns"
      (test "Basic quasipattern matches" 1
        (match '(x 1)
          (`(x ,n) n)
          (_ 'fail)))
      (test "Basic quasipattern doesn’t match" #f
        (match '(y 1)
          (`(x ,n) n)
          (_ #f)))

      (test-error "Empty unquote not in splicing context is a syntax violation" syntax-violation?
        (eval
         '(match 'x
            (`(unquote) #t)
            (_ #f))
         test-env))
      (test "Single-form unquote outside of splicing context works" 'x
        (match 'x
          (`,v v)
          (_ #f)))
      (test-error "Multi-subform unquote not in splicing context is a syntax violation" syntax-violation?
        (eval
         '(match 'x
            (`(unquote x y) #t)
            (_ #f))
         test-env))

      (test-error "unquote-splicing not in splicing context is a syntax violation" syntax-violation?
        (eval
         '(match '(1 2 3)
            (`,@x #t)
            (_ #f))
         test-env))

      (test "Empty unquote in splicing context is equivalent to nothing" #t
        (match '(1 2 3)
          (`(1 (unquote) 2 3) #t)
          (_ #f)))
      (test "Single-form unquote in splicing context works" 1
        (match '(1 2 3)
          (`(,x 2 3) x)
          (_ #f)))
      (test "Multi-subform unquote in splicing context works" '(3 2 1)
        (match '(1 2 3)
          (`((unquote x y z)) (list z y x))
          (_ #f)))

      (test "Empty unquote in splicing context is equivalent to nothing" #t
        (match '(1 2 3)
          (`(1 (unquote-splicing) 2 3) #t)
          (_ #f)))
      (test "Single-form unquote-splicing is equivalent to ellipsis" '(2 3)
        (match '(1 2 3)
          (`(1 ,@x) x)
          (_ #f)))

      (test "Ellipsis works as an alternative to unquote-splicing" #t
        (match '(a a b)
          (`(a ... b) #t)
          (_ #f)))
      (test "Extended ellipsis works" #t
        (match '(a a b)
          (`(a (... 1 2) b) #t)
          (_ #f)))
      (test-error "Ellipsis not in splicing context is a syntax violation" syntax-violation?
        (eval
         '(match 'x
            (`... #t)
            (_ #f))
         test-env))
      (test-error "Extended ellipsis not in splicing context is a syntax violation" syntax-violation?
        (eval
         '(match 'x
            (`(... 1 2) #t)
            (_ #f))
         test-env))

      (test "Tail unquote works" '(b c)
        (match '(a b c)
          (`(a . ,x) x)
          (_ #f)))
      (test "Tail unquote-splicing works" '(b c)
        (match '(a b c)
          (`(a . ,@x) x)
          (_ #f)))

      (test "Vector quasipattern works" '(b c)
        (match '#(a b c)
          (`#(a ,@x) x)
          (_ 'fail)))

      (test "Nested quasipattern" 'not-bound
        (let ((c 'not-bound))
          (match `(a `(b ,c))
            (`(a `(b ,c)) c)
            (_ #f)))))

    (test-group "define-pattern-syntax"
      (let ()
        (define-syntax always (syntax-rules ()))
        (define-pattern-syntax always
          (syntax-rules () ((_) _)))
        (define-syntax never (syntax-rules ()))
        (define-pattern-syntax never
          (syntax-rules () ((_) (not _))))

        (test "Basic pattern syntax matches" #t
          (match 'x
            ((always) #t)
            (_ #f)))
        (test "Basic pattern syntax doesn’t match" #f
          (match 'x
            ((never) #t)
            (_ #f))))

      (let ()
        (define-syntax fizz (syntax-rules ()))
        (define-pattern-syntax fizz
          (syntax-rules ()
            ((_)
             (? number?
                (=> (lambda (x) (mod x 3)) 0)))))
        (test "More complex pattern syntax matches" #t
          (match 15
            ((fizz) #t)
            (_ #f)))
        (test "More complex pattern syntax doesn’t match" #f
          (match 11
            ((fizz) #t)
            (_ #f))))

      (let ()
        (define-record-type pare (fields kar kdr))
        (define-pattern-syntax pare
          (syntax-rules ()
            ((_ a b)
             (? pare?
                (=> pare-kar a)
                (=> pare-kdr b)))))

        (test "Pattern syntax can bind variables" '(5 7)
          (match (make-pare 5 7)
            ((pare x y) (list x y))))
        (test "Pattern syntax can use other syntax inside it" #t
          (match (make-pare 1 2)
            ((pare (? odd?) (? even?)) #t)
            (_ #t)))
        (test "Pattern syntax can be used inside other syntax" '(a d)
          (match (cons (make-pare 'a 'd) '())
            ((? pair?
                (=> car (pare h t))
                (=> cdr '()))
             (list h t))
            (_ #f))))

      (test-error "Exception raised by pattern syntax transformer is passed through"
        (lambda (err)
          (and (syntax-violation? err)
               (eqv? (syntax-violation-form err) 42)
               (eqv? (syntax-violation-subform err) 6)
               (eq? (condition-who err) 'the-will-error-transformer)
               (equal? (condition-message err) "foo!")))
        (eval '(let ()
                 (define-syntax will-error (syntax-rules ()))
                 (define-pattern-syntax will-error
                   (lambda (stx)
                     (syntax-violation 'the-will-error-transformer
                                       "foo!"
                                       42
                                       6)))
                 (match 12
                   ((will-error) #f)))
              test-env))

      (test-values "Local pattern syntax" (values #t #t #t)
        (let ((outer-before-val #f) (inner-val #f))
          (define-syntax fred (syntax-rules ()))
          (define-pattern-syntax fred (syntax-rules () ((_) 1)))
          (set! outer-before-val (match 1
                                   ((fred) #t)
                                   (_ #f)))
          (let ()
            (define-pattern-syntax fred (syntax-rules () ((_) 2)))
            (set! inner-val (match 2
                              ((fred) #t)
                              (_ #f))))
          (values outer-before-val
                  inner-val
                  (match 1
                    ((fred) #t)
                    (_ #f)))))

      (test-values "Local redefinition of global patterns" (values #t #t)
        (let ((inner-val #f))
          (let ()
            (define-pattern-syntax cons (syntax-rules () ((_ _ _) 4)))
            (set! inner-val (match 4
                              ((cons a b) #t)
                              (_ #f))))
          (values inner-val
                  (match '(1 . 2)
                    ((cons _ _) #t)
                    (_ #f)))))

      (test-error "Unknown pattern syntax is a syntax error" syntax-violation?
        (eval
         '(match 'x
            ((nonexistent) #t)
            (_ #f))
         test-env))

      (test-group "Okasaki’s balancing pattern"
        (let ()
          (define-record-type (Node node node?)
            (fields (immutable colour node-colour)
                    (immutable left node-left)
                    (immutable value node-value)
                    (immutable right node-right)))
          (define-pattern-syntax node
            (syntax-rules ()
              ((_ c l v r)
               (? node?
                  (=> node-colour c)
                  (=> node-left l)
                  (=> node-value v)
                  (=> node-right r)))))
          (define (node=? a b)
            (or (eq? a b)
                (and (node? a)
                     (node? b)
                     (eq? (node-colour a) (node-colour b))
                     (node=? (node-left a) (node-left b))
                     (eqv? (node-value a) (node-value b))
                     (node=? (node-right a) (node-right b)))))
          (define (balance n)
            (match n
              ((or (node 'black (node 'red (node 'red a x b) y c) z d)
                   (node 'black (node 'red a x (node 'red b y c)) z d)
                   (node 'black a x (node 'red (node 'red b y c) z d))
                   (node 'black a x (node 'red b y (node 'red c z d))))
               (node 'red (node 'black a x b) y (node 'black c z d)))
              (_ n)))
          (define balanced-node (node 'red
                                      (node 'black 'a 1 'b)
                                      2
                                      (node 'black 'c 3 'd)))

          (test-equal node=? "left-left red violation"
            balanced-node
            (balance (node 'black
                           (node 'red
                                 (node 'red 'a 1 'b)
                                 2
                                 'c)
                           3
                           'd)))
          (test-equal node=? "left-right red violation"
            balanced-node
            (balance (node 'black
                           (node 'red
                                 'a
                                 1
                                 (node 'red 'b 2 'c))
                           3
                           'd)))
          (test-equal node=? "right-left red violation"
            balanced-node
            (balance (node 'black
                           'a
                           1
                           (node 'red
                                 (node 'red 'b 2 'c)
                                 3
                                 'd))))
          (test-equal node=? "right-right red violation"
            balanced-node
            (balance (node 'black
                           'a
                           1
                           (node 'red
                                 'b
                                 2
                                 (node 'red 'c 3 'd)))))

          (test-equal node=? "no violation"
            balanced-node
            (balance balanced-node)))))

    (test-group "match-ellipsis?"
      (test "Single ellipsis is match ellipsis" #t
        (match-ellipsis? #'(... ...)))

      (test "Identifier other than an ellipsis isn’t match ellipsis" #f
        (match-ellipsis? #'foo))

      (test "Extended ellipsis for exactly n repetitions is match ellipsis" #t
        (match-ellipsis? #'(... (... 5))))

      (test-error "Exactly n repetitions must be an exact integer literal" syntax-violation?
        (match-ellipsis? #'(... (... 5.0))))

      (test "Extended ellipsis for minimum n repetitions is match ellipsis" #t
        (match-ellipsis? #'(... (... 5 #t))))

      (test-error "Minimum n repetitions must be exact integer literal" syntax-violation?
        (match-ellipsis? #'(... (... 5.0 #t))))

      (test "Extended ellipsis for between m and n is match ellipsis" #t
        (match-ellipsis? #'(... (... 1 5))))

      (test-error "m must be an exact integer literal" syntax-violation?
        (match-ellipsis? #'(... (... 1.0 5))))

      (test-error "n must be an exact integer literal" syntax-violation?
        (match-ellipsis? #'(... (... 1 5.0))))

      (test-error "Miscellaneous ellipsis abuse" syntax-violation?
        (match-ellipsis? #'(... (... . foo))))

      (test "Other pattern syntax isn’t match ellipsis" #f
        (match-ellipsis? #'(cons a b)))

      (test "Neither pattern syntax nor match ellipsis" #f
        (match-ellipsis? #'((cons a b) (cons c d)))))

    (test-group "Syntactic sugar"
      (test "match-values" '(1 . 2)
        (match-values (values 1 2)
          ((a b) (cons a b))))

      (test-error "match-values errors with wrong number of patterns"
        (match-values (values 1 2)
          ((a) #f)))

      (test-error "Syntax error for disjointed variables in match-values" syntax-violation?
        (eval
         '(let ((a #f))
            (match-values (values 1 2)
              (((or a _) 2) a)
              ((_ _) #f)))
         test-env))

      (test "match-let" '(1 2 3)
        (match-let (((cons a b) '(1 . 2))
                    ((and c (? number?)) 3))
          (list a b c)))

      (test "match-let evaluates out of scope of pattern variables" 'outer
        (let ((v 'outer))
          (match-let (((cons v _) '(inner . _))
                      (q v))
            q)))

      (test-error "match-let raises syntax error on disjointed variables" syntax-violation?
        (eval
         '(let ((a #f))
            (match-let (((or a _) 1))
              a))
         test-env))

      (test "match-let* evaluates in scope of pattern variables" 'inner
        (let ((v 'outer))
          (match-let* (((cons v _) '(inner . _))
                       (q v))
            q)))

      (test-error "match-let* raises syntax error on disjointed variables" syntax-violation?
        (eval
         '(let ((a #f))
            (match-let* ((a 1)
                         ((or a _) 2))
              a))
         test-env))

      (test "match-let-values" '(1 2 3 4)
        (match-let-values ((((cons a b) (cons c d))
                            (values '(1 . 2) '(3 . 4))))
          (list a b c d)))

      (test "match-let-values evaluates out of scope of pattern variables" 'outer
        (let ((v 'outer))
          (match-let-values ((((cons v w) (cons g m))
                              (values '(inner . _) '(_ . _)))
                             ((q) v))
            q)))

      (test-error "match-let-values raises syntax error on disjointed variables" syntax-violation?
        (eval
         '(let ((a #f))
            (match-let-values (((or a _) _)
                               (values 1 2))
              a))
         test-env))

      (test "match-let*-values evaluates in of scope of pattern variables" 'inner
        (let ((v 'outer))
          (match-let*-values ((((cons v w) (cons g m))
                               (values '(inner . _) '(_ . _)))
                              ((q) v))
            q)))

      (test-error "match-let*-values raises syntax error on disjointed variables" syntax-violation?
        (eval
         '(let ((a #f))
            (match-let-values (((a) #t)
                               ((or a _) _)
                               (values 1 2))
              a))
         test-env))

      (test "match-define" 2
        (let ()
          (match-define (cons _ a) '(1 . 2))
          a))

      (test-error "match-define raises syntax error on disjointed variables" syntax-violation?
        (eval
         '(let ((a #f))
            (match-define (or a _) 1)
            a)
         test-env))

      (test-values "match-letrec allows mutually-recursive definitions" (values #t #f)
        (match-letrec (((cons matched-even? matched-odd?)
                        (cons
                         (lambda (x) (or (= x 0) (matched-odd? (- x 1))))
                         (lambda (x) (not (matched-even? x))))))
          (values (matched-even? 2) (matched-even? 3))))

      ;; first check that the Scheme implementation actually enforces
      ;; the letrec restriction, and skip the next test if not
      (parameterize ((current-test-removers
                      (guard
                          (exc (else (list (lambda ignore #f))))
                        (and
                         (eval
                          '(letrec ((x 1) (y x)) y)
                          test-env)
                         (list (lambda ignore #t))))))
        (test-error "match-letrec enforces the letrec restriction"
          (eval
           '(match-letrec
                (((cons x y) '(5 . 7))
                 ((cons sum product) (cons (+ x y) (* x y))))
              (values sum product))
           test-env)))

      (test-error "match-letrec raises syntax error on disjointed variables" syntax-violation?
        (eval
         '(let ((a #f))
            (match-letrec (((or a _) 1))
              a))
         test-env))

      (test-values "match-letrec* allows mutually-recursive definitions" (values #t #f)
        (match-letrec* (((cons matched-even? matched-odd?)
                         (cons
                          (lambda (x) (or (= x 0) (matched-odd? (- x 1))))
                          (lambda (x) (not (matched-even? x))))))
          (values (matched-even? 2) (matched-even? 3))))

      (test-values "match-letrec* does not enforce the letrec restriction" (values 12 35)
        ;; use eval for this one too, because if the restriction is
        ;; incorrectly enforced, it’s likely to be an expand-time
        ;; error not a run-time one, which would stop the whole test
        ;; suite running
        (eval
         '(match-letrec*
              (((cons x y) '(5 . 7))
               ((cons sum product) (cons (+ x y) (* x y))))
            (values sum product))
         test-env))

      (test-error "match-letrec* raises syntax error on disjointed variables" syntax-violation?
        (eval
         '(let ((a #f))
            (match-letrec* (((or a _) 1))
              a))
         test-env))

      (test "if-match succeeds" 3
        (if-match (((cons a b) '(1 . 2)))
                  (+ a b)
                  #f))

      (test "if-match fails" #f
        (if-match (((cons a b) 'not-a-pair))
                  (+ a b)
                  #f))

      (test-error "if-match doesn’t allow disjointed variables when it succeeds" syntax-violation?
        (eval
         '(let ((a #f))
            (if-match (((or a _) 2))
              a
              'failed))
         test-env)))))

;; Local Variables:
;; eval: (put 'test 'scheme-indent-function 2)
;; eval: (put 'test-equal 'scheme-indent-function 'defun)
;; eval: (put 'test-error 'scheme-indent-function 'defun)
;; eval: (put 'test-group 'scheme-indent-function 1)
;; eval: (put 'test-values 'scheme-indent-function 2)
;; End:
