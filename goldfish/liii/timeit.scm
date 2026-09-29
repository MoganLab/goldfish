(define-library (liii timeit)
  (import (goldfish))
  (export timeit)
  (import (liii base) (scheme base) (scheme time))
  (begin

    (define* (timeit stmt (setup '()) (number 1000000))
      (unless (procedure? stmt)
        (error 'type-error "(timeit stmt setup number): stmt must be a procedure")
      ) ;unless
      (unless (or (procedure? setup) (null? setup))
        (error 'type-error
          "(timeit stmt setup number): setup must be a procedure or '()"
        ) ;error
      ) ;unless
      (unless (and (integer? number) (positive? number))
        (error 'type-error
          "(timeit stmt setup number): number must be a positive integer"
        ) ;error
      ) ;unless

      (unless (null? setup)
        (setup)
      ) ;unless

      (let ((start-time (monotonic-nanosecond)))
        (do ((i 0 (+ i 1)))
          ((= i number))
          (stmt)
        ) ;do

        (/ (- (monotonic-nanosecond) start-time) 1000000000)
      ) ;let
    ) ;define*

  ) ;begin
) ;define-library
