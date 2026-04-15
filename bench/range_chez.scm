(import (chezscheme))

(map (lambda (y)
       (filter (lambda (z) (zero? (modulo z 3)))
         (map (lambda (x) (* x x)) (iota 10000))
       ) ;filter
     ) ;lambda
  (iota 10000)
) ;map

(define start-time (current-time))
(define result
  (map (lambda (y)
         (filter (lambda (z) (zero? (modulo z 3)))
           (map (lambda (x) (* x x)) (iota 10000))
         ) ;filter
       ) ;lambda
    (iota 1000)
  ) ;map
) ;define
(define end-time (current-time))

(display "Length of each result: ")
(display (length (car result)))
(newline)

(display "Elapsed time: ")
(display (time-difference end-time start-time)
) ;display
