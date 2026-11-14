;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code config query)
  (import (scheme base) (liii list))
  (export pi-cfg-ref pi-cfg-set pi-cfg-merge)
  (begin

    (define (pi-cfg-ref cfg key . default-val)
      (let ((fallback (if (pair? default-val) (car default-val) #f)))
        (if (pair? cfg)
          (let ((pair (assoc key cfg)))
            (if pair (cdr pair) fallback)
          ) ;let
          fallback
        ) ;if
      ) ;let
    ) ;define

    (define (pi-cfg-set cfg key val)
      (let ((base (if (pair? cfg) cfg '())))
        (cons (cons key val) (filter (lambda (p) (not (eq? (car p) key))) base))
      ) ;let
    ) ;define

    (define (pi-cfg-merge cfg overrides)
      (fold (lambda (pair acc) (pi-cfg-set acc (car pair) (cdr pair))) cfg overrides)
    ) ;define

  ) ;begin
) ;define-library
