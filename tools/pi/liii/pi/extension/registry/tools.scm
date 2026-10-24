;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension registry tools)
  (import (scheme base)
    (liii error)
    (liii list)
    (liii pi extension types)
    (liii pi extension registry base)
    (liii pi ai tools record)
  ) ;import
  (export normalize-tool-def register-tool! extension-register-tool!
    find-extension-tool extension-registry-tools-list
  ) ;export
  (begin

    (define (normalize-tool-def def)
      (cond ((extension-tool-def? def) def)
            ((pi-tool-definition? def)
             (make-extension-tool-def (pi-tool-definition-name def)
               (pi-tool-definition-description def)
               (pi-tool-definition-parameters def)
               (pi-tool-definition-execute def)
               :snippet    (pi-tool-definition-snippet def)
               :guidelines (pi-tool-definition-guidelines def)
             ) ;make-extension-tool-def
            ) ;
            ((pair? def)
             (let ((name (cond ((assq 'name def) => cdr) (else #f)))
                   (desc (cond ((assq 'description def) => cdr) (else #f)))
                   (params (cond ((assq 'parameters def) => cdr) (else '())))
                   (exec (cond ((assq 'execute def) => cdr) (else #f)))
                   (snip (cond ((assq 'snippet def) => cdr) (else "")))
                   (guides (cond ((assq 'guidelines def) => cdr) (else '())))
                  ) ;
               (make-extension-tool-def name desc params exec :snippet snip :guidelines guides)
             ) ;let
            ) ;
            (else (type-error "tool-def must be an extension-tool-def or alist"))
      ) ;cond
    ) ;define

    (define (register-tool! target tool-def)
      (let* ((reg (target->registry target))
             (tool (normalize-tool-def tool-def))
             (tools (extension-registry-tools reg))
             (updated (upsert-by-key tools extension-tool-def-name tool))
            ) ;
        (set-extension-registry-tools! reg updated)
        tool
      ) ;let*
    ) ;define

    (define extension-register-tool! register-tool!)

    (define (find-extension-tool target name)
      (let* ((reg (target->registry target)) (tools (extension-registry-tools reg)))
        (find (lambda (t) (string=? (extension-tool-def-name t) name)) tools)
      ) ;let*
    ) ;define

    (define (extension-registry-tools-list target)
      (extension-registry-tools (target->registry target))
    ) ;define

  ) ;begin
) ;define-library
