;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi agent tools-compat)
  (import (scheme base) (liii json) (liii list) (liii pi ai message))
  (export %maybe-json-block %maybe-json-msg %to-block-list)
  (begin

    (define (%assoc-val key alt-key obj default-val)
      (let ((pair (or (assoc key obj) (assoc alt-key obj))))
        (if (pair? pair) (cdr pair) default-val)
      ) ;let
    ) ;define

    (define (%json-dict? obj)
      (and (json-object? obj)
        (or (equal? obj '(()))
          (every
            (lambda (p) (and (pair? p) (string? (car p))))
            obj
          ) ;every
        ) ;or
      ) ;and
    ) ;define

    (define (%maybe-json-block x)
      (if (%json-dict? x)
        (or (json->pi-block x) x)
        (let ((t (and (pair? x) (%assoc-val 'type "type" x #f))))
          (cond ((equal? t "text") (pi-text-block (%assoc-val 'text "text" x "")))
                ((equal? t "toolCall")
                 (pi-tool-call-block (%assoc-val 'id "id" x "")
                   (%assoc-val 'name "name" x "")
                   (%assoc-val 'arguments "arguments" x '(()))
                 ) ;pi-tool-call-block
                ) ;
                (else x)
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (%maybe-json-msg x)
      (if (%json-dict? x) (or (json->pi-message x) x) x)
    ) ;define

    (define (%to-block-list c)
      (map %maybe-json-block
        (cond ((vector? c) (vector->list c))
              ((pair? c) c)
              ((string? c) (list (pi-text-block c)))
              (else '())
        ) ;cond
      ) ;map
    ) ;define

  ) ;begin
) ;define-library
