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
  ;; Scope sets are duplicate-free lists, so the first match is the only
  ;; match: stop the scan as soon as it is found.  expand-macro-once's
  ;; output flip removes the intro scope that stx-ctx-add-then-flip just
  ;; consed at the head of the set, so this is O(1) on the hot path.
  (let loop ((rest st) (acc '()))
    (cond
      ((null? rest) (reverse acc))
      ((eq? (car rest) element) (append (reverse acc) (cdr rest)))
      (else (loop (cdr rest) (cons (car rest) acc))))))

(define (set-flip st element)
  (if (set-member? st element)
      (set-remove st element)
      (cons element st)))

(define (set-union st1 st2)
  (set-fold set-add st1 st2))

(define (set-subtract st1 st2)
  (set-fold set-remove st1 st2))

(define set-fold fold)

