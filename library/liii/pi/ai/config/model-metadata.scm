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
    pi-model-metadata-matched? pi-model-metadata->alist pi-model-metadata->json
    pi-model-metadata-from-alist
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

    (define (pi-model-metadata->alist m)
      (if (pair? m)
        m
        (list (cons 'id (pi-model-metadata-id m))
          (cons 'name (pi-model-metadata-name m))
          (cons 'provider (pi-model-metadata-provider m))
          (cons 'base-url (pi-model-metadata-base-url m))
          (cons 'context-window (pi-model-metadata-context-window m))
          (cons 'max-tokens (pi-model-metadata-max-tokens m))
          (cons 'matched? (pi-model-metadata-matched? m))
        ) ;list
      ) ;if
    ) ;define

    (define (pi-model-metadata->json m)
      (if (pair? m)
        m
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
      ) ;if
    ) ;define

    ;; alist → record：会话从 JSON 载入后把旧形态的 model-meta 规范化为
    ;; record；已是 record 则原样返回。字段容错：缺失或 #f 时给安全默认值。
    (define (%meta-field alist key default)
      (let ((p (assoc key alist)))
        (if (and p (cdr p)) (cdr p) default)
      ) ;let
    ) ;define

    (define (pi-model-metadata-from-alist m)
      (if (pi-model-metadata? m)
        m
        (and (pair? m)
          (pi-model-metadata (%meta-field m 'id "")
            (%meta-field m 'name "")
            (%meta-field m 'provider "")
            (%meta-field m 'base-url "")
            (%meta-field m 'context-window #f)
            (%meta-field m 'max-tokens #f)
            (%meta-field m 'matched? #t)
          ) ;pi-model-metadata
        ) ;and
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
