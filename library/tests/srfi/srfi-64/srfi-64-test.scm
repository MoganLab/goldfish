(import (scheme base)
        (scheme write)
        (srfi srfi-64))

(define (triv-runner thunk)
  (let ((r (test-runner-null))
        (accum-pass '())
        (accum-fail '())
        (accum-xfail '())
        (accum-xpass '())
        (accum-skip '()))
    (test-runner-on-bad-count!
     r
     (lambda (runner count expected-count)
       (error "bad count" count expected-count)))
    (test-runner-on-bad-end-name!
     r
     (lambda (runner begin end)
       (error "bad end group name" end begin)))
    (test-runner-on-test-end!
     r
     (lambda (runner)
       (let ((n (test-runner-test-name runner)))
         (case (test-result-kind runner)
           ((pass) (set! accum-pass (cons n accum-pass)))
           ((fail) (set! accum-fail (cons n accum-fail)))
           ((xpass) (set! accum-xpass (cons n accum-xpass)))
           ((xfail) (set! accum-xfail (cons n accum-xfail)))
           ((skip) (set! accum-skip (cons n accum-skip)))))))
    (test-with-runner r (thunk))
    (list (reverse accum-pass)
          (reverse accum-fail)
          (reverse accum-xfail)
          (reverse accum-xpass)
          (reverse accum-skip)
          (list (test-runner-pass-count r)
                (test-runner-fail-count r)
                (test-runner-xfail-count r)
                (test-runner-xpass-count r)
                (test-runner-skip-count r)))))

(define main-runner (test-runner-null))
(test-runner-on-test-end!
 main-runner
 (lambda (runner)
   (let ((kind (test-result-kind runner)))
     (when (memq kind '(fail xpass))
       (display "FAIL: ")
       (display (test-runner-test-name runner))
       (newline)))))
(test-runner-on-final!
 main-runner
 (lambda (runner)
   (when (or (> (test-runner-fail-count runner) 0)
             (> (test-runner-xpass-count runner) 0))
     (error "SRFI-64 tests failed"
            (test-runner-fail-count runner)
            (test-runner-xpass-count runner)))))

(test-with-runner main-runner
  (test-begin "SRFI-64")

  ;; 1. Simple test-cases
  (test-equal
   "1.1. test-assert"
   '(("a") ("b" "") () () () (1 2 0 0 0))
   (triv-runner
    (lambda ()
      (test-assert "a" #t)
      (test-assert "b" #f)
      (test-assert #f))))

  (test-equal
   "1.2. test-eqv / test-eq / test-equal / test-approximate"
   '(("eqv-pass" "eq-pass" "equal-pass" "approx-pass")
     ("eqv-fail" "approx-fail")
     () () () (4 2 0 0 0))
   (triv-runner
    (lambda ()
      (test-eqv "eqv-pass" 42 (+ 40 2))
      (test-eqv "eqv-fail" 4.0 4)
      (test-eq "eq-pass" 'foo 'foo)
      (test-equal "equal-pass" '(1 2 3) (list 1 2 3))
      (test-approximate "approx-pass" 3.14 3.14159 0.01)
      (test-approximate "approx-fail" 3.0 3.5 0.1))))

  ;; 2. Catching errors
  (test-equal
   "2.1. test-assert error & test-error"
   '(("err-pass" "err-typed") ("err-in-assert" "err-fail") () () () (2 2 0 0 0))
   (triv-runner
    (lambda ()
      (test-assert "err-in-assert" (vector-ref '#(1 2) 5))
      (test-error "err-pass" #t (vector-ref '#(1 2) 5))
      (test-error "err-fail" #t (vector-ref '#(1 2) 0))
      (test-error "err-typed" #t (error "boom")))))

  ;; 3. Groups, paths, and cleanup
  (test-equal
   "3.1. test-runner-group-path"
   '((() "w") (("a" "b") "x") (("a") "z"))
   (let ((paths '())
         (r (test-runner-null)))
     (test-runner-on-test-end!
      r
      (lambda (runner)
        (set! paths (cons (list (test-runner-group-path runner)
                                (test-runner-test-name runner))
                          paths))))
     (test-with-runner r
       (test-assert "w" #t)
       (test-begin "a")
       (test-begin "b")
       (test-assert "x" #t)
       (test-end)
       (test-assert "z" #t)
       (test-end "a"))
     (reverse paths)))

  (test-equal
   "3.2. test-group-with-cleanup"
   '(body clean)
   (let ((log '()))
     (triv-runner
      (lambda ()
        (test-group-with-cleanup
         "cleanup"
         (set! log (cons 'body log))
         (set! log (cons 'clean log)))))
     (reverse log)))

  ;; 4. Specifiers
  (test-equal
   "4.1. test-skip & test-expect-fail"
   '(("y") () ("xf") ("xp") ("x" "z") (1 0 1 1 2))
   (triv-runner
    (lambda ()
      (test-skip (test-match-name "x"))
      (test-skip (test-match-nth 3))
      (test-assert "x" #t)
      (test-assert "y" #t)
      (test-assert "z" #t)
      (test-expect-fail "xf")
      (test-expect-fail "xp")
      (test-assert "xf" #f)
      (test-assert "xp" #t))))

  (test-equal
   "4.2. test-match-all & test-match-any"
   '(("b") () () () ("a" "c" "d") (1 0 0 0 3))
   (triv-runner
    (lambda ()
      (test-skip (test-match-all (test-match-nth 1 2) (test-match-name "a")))
      (test-skip (test-match-any "c" (test-match-nth 4)))
      (test-assert "a" #t)
      (test-assert "b" #t)
      (test-assert "c" #t)
      (test-assert "d" #t))))

  ;; 5. test-apply
  (test-equal
   "5.1. test-apply"
   '(("w" "p" "v") () () () ("x") (3 0 0 0 1))
   (triv-runner
    (lambda ()
      (test-begin "suite")
      (test-assert "w" #t)
      (test-apply
       (test-match-name "p")
       (lambda ()
         (test-assert "x" #t)
         (test-assert "p" #t)))
      (test-assert "v" #t)
      (test-end "suite"))))

  ;; 6. Result properties & test-read-eval-string
  (test-equal
   "6.1. result properties"
   '(missing 99 pass #t 3 3)
   (let ((r (test-runner-null))
         (res #f))
     (test-runner-on-test-end!
      r
      (lambda (runner)
        (set! res (list (test-result-kind runner)
                        (and (test-passed? runner) #t)
                        (test-result-ref runner 'expected-value)
                        (test-result-ref runner 'actual-value)))
        (test-result-set! runner 'custom 99)
        (set! res (cons (test-result-ref runner 'custom) res))
        (test-result-remove runner 'custom)
        (set! res (cons (test-result-ref runner 'custom 'missing) res))))
     (test-with-runner r
       (test-equal "sum" 3 (+ 1 2)))
     res))

  (test-equal "6.2. test-read-eval-string" 6 (test-read-eval-string "(+ 1 2 3)"))
  (test-assert "6.3. test-runner? null" (test-runner? (test-runner-null)))
  (test-assert "6.4. test-runner? simple" (test-runner? (test-runner-simple)))

  (test-end "SRFI-64"))
