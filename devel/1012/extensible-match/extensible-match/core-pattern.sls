(library (extensible-match core-pattern)
  (export core:var core:wildcard
          core:quote
          core:and core:or
          core:row core:subject
          core:not core:? core:apply
          core:seq
          core/seq:one core/seq:many
          core/seq:ordered core/seq:partial core/seq:unordered
          core-pattern-case)
  (import (rnrs (6))
          (only (srfi :1 lists)
                append-map
                lset= lset-intersection lset-union lset-difference)
          (extensible-match util))

  ;; In order to allow the public core matching primitives to be
  ;; locally extended, they are all defined as expanders which map to
  ;; this small set of non-exported actual core matching primitives.
  ;; This also simplifies the ‘real’ core significantly.

  (define-syntax core:var (syntax-rules ()))
  (define-syntax core:wildcard (syntax-rules ()))
  (define-syntax core:quote (syntax-rules ()))
  (define-syntax core:and (syntax-rules ()))
  (define-syntax core:or (syntax-rules ()))
  (define-syntax core:row (syntax-rules ()))
  (define-syntax core:subject (syntax-rules ()))
  (define-syntax core:not (syntax-rules ()))
  (define-syntax core:? (syntax-rules ()))
  (define-syntax core:apply (syntax-rules ()))
  (define-syntax core:seq (syntax-rules ()))

  (define-syntax core/seq:one (syntax-rules ()))
  (define-syntax core/seq:many (syntax-rules ()))
  (define-syntax core/seq:ordered (syntax-rules ()))
  (define-syntax core/seq:partial (syntax-rules ()))
  (define-syntax core/seq:unordered (syntax-rules ()))

  (define-syntax core-pattern-case
    (lambda (stx)
      (syntax-case stx ()
        ((k stx (literals ...) clauses ...)
         ;; We can’t just expand into
         ;; #'(syntax-case stx (core:quote ...) ...)
         ;; because syntax-case uses bound-identifier=? to find
         ;; literals from the literals list within its patterns, and
         ;; doing this would put different marks on the literals in
         ;; the literals list to those in the patterns. What we
         ;; ideally want is the ability to set the literals list using
         ;; free-identifier=?, but this would probably mean fairly
         ;; heavy reprocessing of the patterns. This method assumes
         ;; that the core patterns are imported and available under
         ;; their own names in the context where core-pattern-case is
         ;; used – which is a safe assumption, because this macro is
         ;; only used internally.
         (with-syntax
             (((core-pattern-names ...)
               (map (lambda (name)
                      (let ((id (datum->syntax #'k name)))
                        ;; Check that the above-mentioned
                        ;; assumption actually holds
                        (if (free-identifier=? id
                                               (datum->syntax #'foo name))
                            id
                            (syntax-violation 'core-pattern-case
                                              "can only be used when all the core pattern bindings are available under their original names"
                                              name))))
                    '(core:var
                      core:wildcard
                      core:quote
                      core:and
                      core:or
                      core:row
                      core:subject
                      core:not
                      core:?
                      core:apply
                      core:seq
                      core/seq:one
                      core/seq:many
                      core/seq:ordered
                      core/seq:partial
                      core/seq:unordered))))
           #'(syntax-case stx (core-pattern-names ... literals ...)
               clauses ...)))))))
