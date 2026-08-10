(define (set . elements)
  (fold set-add '() elements))

(define (set-member? st element)
  (memq element st))

(define (set<=? st1 st2)
  (if (null? st1)
    #t
    (if (set-member? st2 (car st1))
      (set<=? (cdr st1) st2)
      #f)))

(define (set=? st1 st2)
  (and (set<=? st1 st2)
       (set<=? st2 st1)))

(define (set-add st element)
  (if (set-member? st element)
    st
    (cons element st)))

(define (set-remove st element)
  (filter (lambda (x) (not (eq? x element)))
          st))

(define (set-flip st element)
  (let ((st* (set-remove st element)))
    (if (eq? st st*)
      (cons element st)
      st*)))

(define (set-union st1 st2)
  (set-fold set-add st1 st2))

(define (set-subtract st1 st2)
  (set-fold set-remove st1 st2))

(define set-fold fold)

