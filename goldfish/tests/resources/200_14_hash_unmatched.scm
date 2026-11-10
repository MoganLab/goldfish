; Unmatched case with #racket character literals
(define (test-unmatched) #
  (display #\(
  (let ((x 10))
    (display x)
    (+ x #\)  ; Missing closing parenthesis