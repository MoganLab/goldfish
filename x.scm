; (display (sc-expand '(define-library (x)
;                        (export answer)
;                        (import (scheme base))
;                        (begin
;                          (define answer 42)
;                          (display (square answer))
;                          (newline)))
;                     #f '(L C) '(L)))

; (define-library (x)
;   (export answer)
;   (import (scheme base))
;   (begin
;     (define answer 42)
;     (display (square answer))
;     (newline)))

; (display (sc-expand '(begin
;                        (import (scheme base))
;                        (display (square 42)))
;                     #f '(L C) '(L)))

; (load "/home/jinser/vie/projet/lang/goldfish/goldfish/scheme/base.scm")
; (module x ()
;   (%primitive-import scheme.base)
;   (display (square 42)))

; (define-library (x)
;   (import (scheme base))
;   (begin (display (square 42)))) ; prints 1764

(import (scheme base))
; (%primitive-import scheme.base)
(display (square 42)) ; unbound

; (display (sc-expand '(begin (import (scheme base))
;                             (display (square 42)))
;                    #f '(E) '(E)))
