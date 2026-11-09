; Valid cases with #racket character literals
(define result (list #
 1 #
 2))
(display #\()
(display #\))
(define (test-char) #\(
  (let ((x 5))
    (display #\)
    x)))