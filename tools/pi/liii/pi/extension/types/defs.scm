;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension types defs)
  (import (scheme base) (liii error))
  (export make-extension-tool-def
    extension-tool-def?
    extension-tool-def-name
    extension-tool-def-description
    extension-tool-def-parameters
    extension-tool-def-execute
    extension-tool-def-snippet
    extension-tool-def-guidelines

    make-extension-flag-def
    extension-flag-def?
    extension-flag-def-flag
    extension-flag-def-description
    extension-flag-def-handler
    extension-flag-def-type
    extension-flag-def-default
    extension-flag-def-key
    extension-flag-def-aliases
    extension-flag-def-takes-arg?
    extension-flag-def-validator

    make-extension-command-def
    extension-command-def?
    extension-command-def-name
    extension-command-def-description
    extension-command-def-handler
    extension-command-def-params

    make-extension-flow-def
    extension-flow-def?
    extension-flow-def-name
    extension-flow-def-predicate
    extension-flow-def-handler
    extension-flow-def-local?
    extension-flow-def-priority
  ) ;export
  (begin

    (define (%must-str who val msg)
      (unless (string? val)
        (type-error (string-append who ": " msg))
      ) ;unless
    ) ;define
    (define (%must-non-empty-str who val msg)
      (unless (and (string? val) (> (string-length val) 0))
        (type-error (string-append who ": " msg))
      ) ;unless
    ) ;define
    (define (%must-proc who val msg)
      (unless (procedure? val)
        (type-error (string-append who ": " msg))
      ) ;unless
    ) ;define

    ;; 1. ExtensionToolDef
    (define-record-type <extension-tool-def>
      (%make-extension-tool-def name description parameters execute snippet
        guidelines
      ) ;%make-extension-tool-def
      extension-tool-def?
      (name extension-tool-def-name)
      (description extension-tool-def-description)
      (parameters extension-tool-def-parameters)
      (execute extension-tool-def-execute)
      (snippet extension-tool-def-snippet)
      (guidelines extension-tool-def-guidelines)
    ) ;define-record-type

    (define* (make-extension-tool-def name
               description
               parameters
               execute
               (snippet "")
               (guidelines '())
             ) ;make-extension-tool-def
      (%must-non-empty-str "make-extension-tool-def"
        name
        "name must be a non-empty string"
      ) ;%must-non-empty-str
      (%must-str "make-extension-tool-def" description "description must be a string")
      (%must-proc "make-extension-tool-def" execute "execute must be a procedure")
      (%make-extension-tool-def name description parameters execute snippet
        guidelines
      ) ;%make-extension-tool-def
    ) ;define*

    ;; 2. ExtensionFlagDef
    (define-record-type <extension-flag-def>
      (%make-extension-flag-def flag description handler type default key
        aliases takes-arg? validator
      ) ;%make-extension-flag-def
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
    ) ;define-record-type

    (define (%derive-key-from-flag flag-str)
      (let loop
        ((i 0) (len (string-length flag-str)))
        (if (and (< i len) (char=? (string-ref flag-str i) #\-))
          (loop (+ i 1) len)
          (if (>= i len) 'flag (string->symbol (substring flag-str i len)))
        ) ;if
      ) ;let
    ) ;define

    (define* (make-extension-flag-def flag
               description
               (handler #f)
               (type 'boolean)
               (default #f)
               (key #f)
               (aliases '())
               (takes-arg? #f)
               (validator #f)
             ) ;make-extension-flag-def
      (%must-non-empty-str "make-extension-flag-def"
        flag
        "flag must be a non-empty string"
      ) ;%must-non-empty-str
      (%must-str "make-extension-flag-def" description "description must be a string")
      (let* ((actual-key
               (if key
                 (if (symbol? key) key (string->symbol (object->string key)))
                 (%derive-key-from-flag flag)
               ) ;if
             ) ;actual-key
             (actual-takes-arg? (or takes-arg? (not (eq? type 'boolean))))
             (actual-aliases (if (list? aliases) aliases '()))
            ) ;
        (%make-extension-flag-def flag description handler type default
          actual-key actual-aliases actual-takes-arg? validator
        ) ;%make-extension-flag-def
      ) ;let*
    ) ;define*

    ;; 3. ExtensionCommandDef
    (define-record-type <extension-command-def>
      (%make-extension-command-def name description handler params)
      extension-command-def?
      (name extension-command-def-name)
      (description extension-command-def-description)
      (handler extension-command-def-handler)
      (params extension-command-def-params)
    ) ;define-record-type

    (define* (make-extension-command-def name description handler (params '()))
      (%must-non-empty-str "make-extension-command-def"
        name
        "name must be a non-empty string"
      ) ;%must-non-empty-str
      (%must-str "make-extension-command-def"
        description
        "description must be a string"
      ) ;%must-str
      (%must-proc "make-extension-command-def" handler "handler must be a procedure")
      (%make-extension-command-def name description handler params)
    ) ;define*

    ;; 4. ExtensionFlowDef
    (define-record-type <extension-flow-def>
      (%make-extension-flow-def name predicate handler local? priority)
      extension-flow-def?
      (name extension-flow-def-name)
      (predicate extension-flow-def-predicate)
      (handler extension-flow-def-handler)
      (local? extension-flow-def-local?)
      (priority extension-flow-def-priority)
    ) ;define-record-type

    (define* (make-extension-flow-def name predicate handler (local? #f) (priority 0))
      (unless (or (string? name) (symbol? name))
        (type-error "make-extension-flow-def: name must be a string or symbol")
      ) ;unless
      (%must-proc "make-extension-flow-def" predicate "predicate must be a procedure")
      (%must-proc "make-extension-flow-def" handler "handler must be a procedure")
      (%make-extension-flow-def name
        predicate
        handler
        (if local? #t #f)
        (if (number? priority) priority 0)
      ) ;%make-extension-flow-def
    ) ;define*

  ) ;begin
) ;define-library
