(import (scheme base) (scheme write))
;; call/cc escapes through nested dynamic-wind bars: the KontFrame winder
;; push/pop path, exercised by the evaluator levers (slimming the frame and
;; its winder storage must preserve these counts exactly).
(define enters 0)
(define leaves 0)
(define (trial i)
  (dynamic-wind
    (lambda () (set! enters (+ enters 1)))
    (lambda ()
      (dynamic-wind
        (lambda () (set! enters (+ enters 1)))
        (lambda () (call/cc (lambda (k) (if (= 0 (modulo i 2)) (k #f) #f))))
        (lambda () (set! leaves (+ leaves 1)))))
    (lambda () (set! leaves (+ leaves 1)))))
(let loop ((i 0)) (if (< i 250000) (begin (trial i) (loop (+ i 1))) #f))
(write enters) (display " ") (write leaves) (newline)
