;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi args query)
  (import (scheme base) (liii list))
  (export pi-args-ref pi-args-set pi-resolve-timeout)
  (begin

    (define (pi-args-ref parsed key . default-val)
      (let ((pair (assoc key parsed)))
        (if pair (cdr pair) (if (pair? default-val) (car default-val) #f))
      ) ;let
    ) ;define

    (define (pi-resolve-timeout parsed cfg)
      (let ((t (pi-args-ref parsed 'timeout)))
        (if (and (number? t) (> t 0))
          t
          (let ((cfg-t (cdr (or (assoc 'timeout cfg) '(#f . 600)))))
            (if (and (number? cfg-t) (> cfg-t 0)) cfg-t 600)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (pi-args-set parsed key val)
      (cons (cons key val) (filter (lambda (p) (not (eq? (car p) key))) parsed))
    ) ;define

  ) ;begin
) ;define-library
