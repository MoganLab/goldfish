(import (scheme time)
        (liii rich-range)
        (liii rich-list)
        (liii lang))

(define (prime1? n)
  (define (iter i)
    (cond ((> (* i i) n) #t)
          ((zero? (modulo n i)) #f)
          (else (iter (+ i 2)))))

  (cond ((< n 2) #f)
        ((= n 2) #t)
        ((even? n) #f)
        (else (iter 3))))

(define (prime2? n)
  (cond ((< n 2) #f)
        ((= n 2) #t)
        ((even? n) #f)
        (else
         ((rich-list :range 3 (+ ($ n :sqrt) 1) 2)
          :forall
          (lambda (i) (not (zero? (modulo n i))))))))

(define (timing msg thunk)
  (let* ((start (current-jiffy))
         (val (thunk))
         (end (current-jiffy)))
    (display* msg (number->string (- end start)) "\n")))

(let1 n 1073729
  (timing "R7RS: " (lambda () ((rich-range 1 100) :for-each (lambda (x) (prime1? n)))))
  (timing "Goldfish: " (lambda () ((rich-range 1 100) :for-each (lambda (x) (prime2? n)))))
  (display* (prime1? n) "\n")
)

; (($ 1 :to 100)
;  :filter prime?
;  :filter (lambda (x) (prime? (+ x 2)))
;  :map (lambda (x) (cons x (+ x 2)))
;  :collect)
