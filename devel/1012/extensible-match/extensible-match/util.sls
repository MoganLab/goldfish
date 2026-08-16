(library (extensible-match util)
  (export datum-hash
          bound-identifier-hash
          generate-identifier
          hash-combine
          bitwise-bit-set
          with*-syntax)
  (import (rnrs))

  ;; Bug #48: Hash functions in R6RS are not actually guaranteed to
  ;; return fixnums, so truncate them to fixnum length (and be careful
  ;; not to overflow that length when combining hash values etc)
  (define (datum-hash x)
    (bitwise-and (equal-hash x)
                 (greatest-fixnum)))
  (define (bound-identifier-hash id)
    (bitwise-and (symbol-hash (syntax->datum id))
                 (greatest-fixnum)))

  (define (generate-identifier)
    (car (generate-temporaries '(_))))

  (define fx+/overflow
    (case-lambda
      ((a b)
       (fx+/overflow a b 0))
      ((a b c)
       (let-values (((r ignored) (fx+/carry a b c)))
         (fxand r (greatest-fixnum))))
      ((a b c d)
       (fx+/overflow a b (fx+/overflow c d 0)))
      ((a b c . more)
       (fx+/overflow a b (apply fx+/overflow c more)))))
  (define hash-combine
    (case-lambda
      ((ia ib)
       (define a (bitwise-and ia (greatest-fixnum)))
       (define b (bitwise-and ib (greatest-fixnum)))
       (fxxor a
              (fx+/overflow
               (bitwise-and #x9e3779b9 (greatest-fixnum))
               b
               (fxarithmetic-shift (fxbit-field a
                                                0
                                                (fx- (fixnum-width) 7))
                                   6)
               (fxarithmetic-shift a -2))))
      ((a b c)
       (hash-combine (hash-combine a b) c))
      ((a b c d)
       (hash-combine (hash-combine (hash-combine a b) c) d))
      ((a b . more)
       (apply hash-combine (hash-combine a b) more))))

  (define (bitwise-bit-set n idx)
    (bitwise-ior n (bitwise-arithmetic-shift 1 idx)))

  (define-syntax with*-syntax
    (syntax-rules ()
      ((_ () body_0 body_1 ...)
       (let () body_0 body_1 ...))
      ((_ (this-binding more-binding ...) body_0 body_1 ...)
       (with-syntax (this-binding)
         (with*-syntax (more-binding ...) body_0 body_1 ...))))))
