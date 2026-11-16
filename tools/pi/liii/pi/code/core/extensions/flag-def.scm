;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core extensions flag-def)
  (import (scheme base) (liii base) (liii error) (liii string))
  (export make-extension-flag-def extension-flag-def? extension-flag-def-flag
    extension-flag-def-description extension-flag-def-handler
    extension-flag-def-type extension-flag-def-default extension-flag-def-key
    extension-flag-def-aliases extension-flag-def-takes-arg?
    extension-flag-def-validator
  ) ;export
  (begin

    (define*-record-type <extension-flag-def>
      (make-extension-flag-def flag
        description
        (handler #f)
        (type 'boolean)
        (default #f)
        (key #f)
        (aliases '())
        (takes-arg? #f)
        (validator #f)
      ) ;make-extension-flag-def
      extension-flag-def?
      (flag extension-flag-def-flag)
      (description extension-flag-def-description)
      (handler extension-flag-def-handler)
      (type extension-flag-def-type)
      (default extension-flag-def-default)
      (key extension-flag-def-key)
      (aliases extension-flag-def-aliases)
      (takes-arg? extension-flag-def-takes-arg?)
      (validator extension-flag-def-validator)
    ) ;define*-record-type

    (define (%derive-key-from-flag flag-str)
      (let ((idx (string-skip flag-str #\-)))
        (if idx (string->symbol (string-drop flag-str idx)) 'flag)
      ) ;let
    ) ;define

    (let ((raw make-extension-flag-def))
      (set! make-extension-flag-def
        (lambda* (flag description
                   (handler #f)
                   (type 'boolean)
                   (default #f)
                   (key #f)
                   (aliases '())
                   (takes-arg? #f)
                   (validator #f)
                 ) ;flag
          (unless (and (string? flag) (> (string-length flag) 0))
            (type-error "make-extension-flag-def: flag must be a non-empty string")
          ) ;unless
          (unless (string? description)
            (type-error "make-extension-flag-def: description must be a string")
          ) ;unless
          (let* ((actual-key
                   (if key
                     (if (symbol? key) key (string->symbol (object->string key)))
                     (%derive-key-from-flag flag)
                   ) ;if
                 ) ;actual-key
                 (actual-takes-arg? (or takes-arg? (not (eq? type 'boolean))))
                 (actual-aliases (if (list? aliases) aliases '()))
                ) ;
            (raw flag description handler type default actual-key actual-aliases
              actual-takes-arg? validator
            ) ;raw
          ) ;let*
        ) ;lambda*
      ) ;set!
    ) ;let

  ) ;begin
) ;define-library
