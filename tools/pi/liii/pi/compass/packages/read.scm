;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compass packages read)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii list)
    (liii path)
    (liii json)
    (liii pi compass types)
  ) ;import
  (export pi-compass-read-text pi-compass-read-json pi-compass-object-keys)
  (begin

    (define (pi-compass-read-text file)
      (if (not (path-file? (path file)))
        #f
        (catch #t (lambda () (path-read-text (path file))) (lambda _ #f))
      ) ;if
    ) ;define

    (define (pi-compass-read-json file)
      (let ((text (pi-compass-read-text file)))
        (if (not (string? text))
          #f
          (catch #t (lambda () (string->json text)) (lambda _ #f))
        ) ;if
      ) ;let
    ) ;define

    (define (pi-compass-object-keys obj)
      (if (pi-compass-object? obj) (map car obj) '())
    ) ;define

  ) ;begin
) ;define-library
