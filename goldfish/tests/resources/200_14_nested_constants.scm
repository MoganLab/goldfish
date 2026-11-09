; Correct nested use with constants
(define *open-char* #\()
(define *close-char* #\))

(define (process-parens x)
  (list *open-char* x *close-char*))

(display #\()
(display #\))
(+ 1 2)