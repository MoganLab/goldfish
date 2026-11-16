;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai config credential)
  (import (scheme base) (liii base))
  (export pi-credential pi-credential? pi-credential-provider pi-credential-key
    pi-credential-type pi-credential->json
  ) ;export
  (begin

    (define*-record-type <pi-credential>
      (pi-credential provider key (type "api_key"))
      pi-credential?
      (provider pi-credential-provider)
      (key pi-credential-key)
      (type pi-credential-type)
    ) ;define*-record-type

    (define (pi-credential->json c)
      (list (cons "provider" (pi-credential-provider c))
        (cons "type" (pi-credential-type c))
        (cons "key" (pi-credential-key c))
      ) ;list
    ) ;define

  ) ;begin
) ;define-library
