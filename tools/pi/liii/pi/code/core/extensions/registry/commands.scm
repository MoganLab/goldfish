;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core extensions registry commands)
  (import (scheme base)
    (liii error)
    (liii list)
    (liii pi code core extensions types)
    (liii pi code core extensions registry base)
  ) ;import
  (export normalize-command-def register-command! extension-register-command!
    find-extension-command extension-registry-commands-list
  ) ;export
  (begin

    (define (normalize-command-def def)
      (cond ((extension-command-def? def) def)
            ((pair? def)
             (let ((name (cond ((assq 'name def) => cdr) (else #f)))
                   (desc (cond ((assq 'description def) => cdr) (else #f)))
                   (handler (cond ((assq 'handler def) => cdr) (else #f)))
                   (params (cond ((assq 'params def) => cdr) (else '())))
                  ) ;
               (make-extension-command-def name desc handler :params params)
             ) ;let
            ) ;
            (else (type-error "command-def must be an extension-command-def or alist"))
      ) ;cond
    ) ;define

    (define (register-command! target command-def)
      (let* ((reg (target->registry target))
             (cmd (normalize-command-def command-def))
             (cmds (extension-registry-commands reg))
             (updated (upsert-by-key cmds extension-command-def-name cmd))
            ) ;
        (set-extension-registry-commands! reg updated)
        cmd
      ) ;let*
    ) ;define

    (define extension-register-command! register-command!)

    (define (find-extension-command target name)
      (let* ((reg (target->registry target)) (cmds (extension-registry-commands reg)))
        (find (lambda (c) (string=? (extension-command-def-name c) name)) cmds)
      ) ;let*
    ) ;define

    (define (extension-registry-commands-list target)
      (extension-registry-commands (target->registry target))
    ) ;define

  ) ;begin
) ;define-library
