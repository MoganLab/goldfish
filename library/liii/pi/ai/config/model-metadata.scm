;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai config model-metadata)
  (import (scheme base) (liii base))
  (export pi-model-metadata pi-model-metadata? pi-model-metadata-id
    pi-model-metadata-name pi-model-metadata-provider pi-model-metadata-base-url
    pi-model-metadata-context-window pi-model-metadata-max-tokens
    pi-model-metadata-matched? pi-model-metadata->json
  ) ;export
  (begin

    (define*-record-type <pi-model-metadata>
      (pi-model-metadata id
        name
        provider
        base-url
        context-window
        max-tokens
        (matched? #t)
      ) ;pi-model-metadata
      pi-model-metadata?
      (id pi-model-metadata-id)
      (name pi-model-metadata-name)
      (provider pi-model-metadata-provider)
      (base-url pi-model-metadata-base-url)
      (context-window pi-model-metadata-context-window)
      (max-tokens pi-model-metadata-max-tokens)
      (matched? pi-model-metadata-matched?)
    ) ;define*-record-type

    (define (pi-model-metadata->json m)
      (let ((base
              (list (cons "id" (pi-model-metadata-id m))
                (cons "name" (pi-model-metadata-name m))
                (cons "provider"
                  (let ((p (pi-model-metadata-provider m)))
                    (if (symbol? p) (symbol->string p) p)
                  ) ;let
                ) ;cons
                (cons "baseUrl" (pi-model-metadata-base-url m))
                (cons "matched" (if (pi-model-metadata-matched? m) 'true 'false))
              ) ;list
            ) ;base
            (cw (pi-model-metadata-context-window m))
            (mt (pi-model-metadata-max-tokens m))
           ) ;
        (append base
          (if cw (list (cons "contextWindow" cw)) '())
          (if mt (list (cons "maxTokens" mt)) '())
        ) ;append
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
