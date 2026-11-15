;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code config types)
  (import (scheme base) (liii list) (liii pi code config user-fs))
  (export make-pi-model-entry pi-model-entry? pi-model-entry-id
    pi-model-entry-name pi-model-entry-base-url pi-model-entry-api
    pi-model-entry-reasoning pi-model-entry-context-window
    pi-model-entry-max-tokens pi-model-entry-extras json->pi-model-entry
    pi-model-entry->json
  ) ;export
  (begin

    (define-record-type <pi-model-entry>
      (%make-pi-model-entry id name base-url api reasoning context-window
        max-tokens extras
      ) ;%make-pi-model-entry
      pi-model-entry?
      (id pi-model-entry-id)
      (name pi-model-entry-name)
      (base-url pi-model-entry-base-url)
      (api pi-model-entry-api)
      (reasoning pi-model-entry-reasoning)
      (context-window pi-model-entry-context-window)
      (max-tokens pi-model-entry-max-tokens)
      (extras pi-model-entry-extras)
    ) ;define-record-type

    (define* (make-pi-model-entry (id #f)
               (name #f)
               (base-url #f)
               (api #f)
               (reasoning 'unset)
               (context-window #f)
               (max-tokens #f)
               (extras '())
             ) ;make-pi-model-entry
      (%make-pi-model-entry id name base-url api reasoning context-window
        max-tokens extras
      ) ;%make-pi-model-entry
    ) ;define*

    (define (%model-known-key? k)
      (let ((s (if (symbol? k) (symbol->string k) k)))
        (member s
          '("id" "name" "baseUrl" "api" "reasoning" "contextWindow" "maxTokens")
        ) ;member
      ) ;let
    ) ;define

    (define (%model-extras obj)
      (if (not (pair? obj))
        '()
        (filter
          (lambda (entry) (and (pair? entry) (not (%model-known-key? (car entry)))))
          obj
        ) ;filter
      ) ;if
    ) ;define

    (define (%normalize-opt-bool val)
      (cond ((eq? val 'true) #t)
            ((eq? val 'false) #f)
            ((boolean? val) val)
            (else 'unset)
      ) ;cond
    ) ;define

    (define (json->pi-model-entry obj)
      (cond ((pi-model-entry? obj) obj)
            ((not (pair? obj)) #f)
            (else
              (let ((id (pi-user-dict-ref obj "id" #f))
                    (name (pi-user-dict-ref obj "name" #f))
                    (base-url (pi-user-dict-ref obj "baseUrl" #f))
                    (api (pi-user-dict-ref obj "api" #f))
                    (reasoning (%normalize-opt-bool (pi-user-dict-ref obj "reasoning" 'unset)))
                    (cw (pi-user-dict-ref obj "contextWindow" #f))
                    (mt (pi-user-dict-ref obj "maxTokens" #f))
                    (extras (%model-extras obj))
                   ) ;
                (%make-pi-model-entry (and (string? id) id)
                  (and (string? name) name)
                  (and (string? base-url) base-url)
                  (and (string? api) api)
                  reasoning
                  (and (number? cw) cw)
                  (and (number? mt) mt)
                  extras
                ) ;%make-pi-model-entry
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (pi-model-entry->json entry)
      (if (pair? entry)
        entry
        (let ((id (pi-model-entry-id entry))
              (name (pi-model-entry-name entry))
              (base-url (pi-model-entry-base-url entry))
              (api (pi-model-entry-api entry))
              (reasoning (pi-model-entry-reasoning entry))
              (cw (pi-model-entry-context-window entry))
              (mt (pi-model-entry-max-tokens entry))
              (extras (pi-model-entry-extras entry))
             ) ;
          (append (if (string? id) (list (cons "id" id)) '())
            (if (string? name) (list (cons "name" name)) '())
            (if (string? base-url) (list (cons "baseUrl" base-url)) '())
            (if (string? api) (list (cons "api" api)) '())
            (if (boolean? reasoning)
              (list (cons "reasoning" (if reasoning 'true 'false)))
              '()
            ) ;if
            (if (number? cw) (list (cons "contextWindow" cw)) '())
            (if (number? mt) (list (cons "maxTokens" mt)) '())
            (if (pair? extras) extras '())
          ) ;append
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
