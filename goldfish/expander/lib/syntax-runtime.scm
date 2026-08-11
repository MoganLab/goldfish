;;; syntax-runtime.scm
;;; Expansion-time runtime for the object-level syntax-case macro
;;; (lib/syntax-case.scm): pattern matching and template instantiation.
;;; The syntax-case FORM is a derived macro in lib/syntax-case.scm; this
;;; file holds the procedures its expansion output calls at transformer
;;; run time:
;;;
;;;   syntax-case-dispatch  run-time clause dispatcher
;;;   pattern-match*        pattern matching (literals via free-identifier=?)
;;;   instantiate           template instantiation (the `syntax' contents)
;;;
;;; This is ordinary object-level R7RS source, expanded by the expander
;;; itself (install-library-file!) and installed before lib/syntax-case.scm
;;; (which references syntax-case-dispatch).  It is written in core forms
;;; only (lambda / if / begin / set! / quote / letrec*) because the derived
;;; forms (let / and / or / cond) live in lib/core-macros.scm, installed
;;; later.  Free procedures (map / assq / ...) resolve via the expander
;;; module's fallback to the host rootlet.

;;; Pattern matching
;;;
;;; Pattern trees.  The pattern a clause compiles to is a stx-tree (see
;;; pattern-tree below): a cons tree whose leaves are syntax objects, so
;;; identifiers keep their definition-site scopes.  Literal membership --
;;; deciding whether a pattern element is a literal or a fresh pattern
;;; variable -- is therefore decided by BINDING identity
;;; (bound-identifier=?), not by source name.  The match.scm new-sym?
;;; trick relies on this: two distinct introduced identifiers that happen
;;; to share a source name (e.g. the generated `p-ls' tmps) must be
;;; treated as different pattern variables, and only a pattern element
;;; bound-identifier=? to a literal-list identifier is a literal.
;;; (R7RS: literals match input identifiers by free-identifier=?.)

(define (ellipsis-datum? x)
  (if (syntax? x) (eq? (syntax-form x) '...) (eq? x '...)))

(define (pattern-leaf-datum p)
  (if (syntax? p) (syntax-form p) p))

;;; pattern-tree : syntax -> stx-tree
;;; Cons/vector tree whose leaves are the syntax objects of the original
;;; pattern (identifiers, numbers, etc. all stay syntax).  Structural
;;; pairs and vectors become real pairs/vectors so the matcher can
;;; destructure them.

(define (pattern-tree stx)
  (if (syntax? stx)
      (let ((form (syntax-form stx)))
        (cond
          ((pair? form)
           (cons (pattern-tree (car form)) (pattern-tree (cdr form))))
          ((vector? form)
           (vector-map pattern-tree form))
          (else stx)))
      (cond
        ((pair? stx)
         (cons (pattern-tree (car stx)) (pattern-tree (cdr stx))))
        ((vector? stx)
         (vector-map pattern-tree stx))
        (else stx))))

;;; literal-identical? : id (list syntax) -> bool
;;; Is the pattern element identifier bound-identifier=? to one of the
;;; literal-list identifiers?

(define (literal-identical? pat literals)
  (let loop ((ls literals))
    (if (null? ls)
        #f
        (if (and (identifier? (car ls)) (bound-identifier=? pat (car ls)))
            #t
            (loop (cdr ls))))))

(define (pattern-variable? pat literals)
  (if (identifier? pat)
      (let ((form (syntax-form pat)))
        (if (eq? form '_)
            #f
            (if (eq? form '...)
                #f
                (not (literal-identical? pat literals)))))
      #f))

(define (pattern-match pattern input literals)
  (letrec* ((bindings (pattern-match* pattern input literals '())))
    (if bindings (reverse bindings) #f)))

(define (literal-matches? pattern input literals)
  (if (syntax? input)
      (if (symbol? (syntax-form input))
          (if (literal-identical? pattern literals)
              (if (current-expand-context)
                  (free-identifier=? input pattern)
                  (eq? (syntax-form input) (syntax-form pattern)))
              (eq? (syntax-form input) (syntax-form pattern)))
          #f)
      #f))

(define (pattern-match* pattern input literals bindings)
  (if (pattern-variable? pattern literals)
      (cons (cons (pattern-leaf-datum pattern) input) bindings)
      (if (and (identifier? pattern) (eq? (syntax-form pattern) '_))
          ;; `_` is a wildcard, but when listed among the literals it matches
          ;; only the identifier `_` itself.  That is the R7RS-literal reading
          ;; portable match libraries rely on to tell a wildcard pattern from
          ;; a binder (match.scm lists `_' among match-two's literals so the
          ;; rule `(match-two v _ ...)' fires only for a literal `_' pattern,
          ;; not for any identifier).  Guile behaves the same way.  (The
          ;; syntax-rules rule HEAD is not `_' -- it is a fresh pattern
          ;; variable -- so it matches any keyword regardless of this list.)
          (if (literal-identical? pattern literals)
              (if (and (syntax? input) (eq? (syntax-form input) '_))
                  bindings
                  #f)
              bindings)
          (if (identifier? pattern)
              (if (literal-matches? pattern input literals)
                  bindings
                  #f)
              ;; Vector pattern: destructure element-wise (R6RS syntax-case
              ;; vector patterns, e.g. match.scm's #(p ...)).
              (if (vector? pattern)
                  (letrec* ((in-form (if (syntax? input)
                                         (syntax-form input)
                                         (if (vector? input) input #f))))
                    (if (and in-form (vector? in-form))
                        (pattern-match-list (vector->list pattern)
                                            (vector->list in-form)
                                            input literals bindings)
                        #f))
                  (if (not (pair? pattern))
                      (if (and (syntax? input)
                               (equal? (syntax-form input) (pattern-leaf-datum pattern)))
                          bindings
                          (if (equal? input (pattern-leaf-datum pattern)) bindings #f))
                      (if (not (syntax? input))
                          #f
                          (if (and (not (pair? (syntax-form input)))
                                   (not (null? (syntax-form input))))
                              #f
                              (pattern-match-list pattern (syntax-form input) input literals bindings)))))))))

(define (pattern-match-list pat-list input-form input-stx literals bindings)
  (if (null? pat-list)
      (if (null? input-form) bindings #f)
      ;; Dotted tail: an improper pattern's terminal atom matches the whole
      ;; remaining input (R7RS 7.3 dotted syntax-case patterns).
      (if (not (pair? pat-list))
          (pattern-match-tail pat-list input-form input-stx literals bindings)
          (if (and (pair? (cdr pat-list))
                   (ellipsis-datum? (cadr pat-list)))
              (pattern-match-ellipsis (car pat-list) (cddr pat-list) input-form input-stx literals bindings)
              (if (not (pair? input-form))
                  #f
                  (letrec* ((bindings2 (pattern-match* (car pat-list) (car input-form) literals bindings)))
                    (if bindings2
                        (pattern-match-list (cdr pat-list) (cdr input-form) input-stx literals bindings2)
                        #f)))))))

;;; pattern-match-tail : match a dotted pattern tail against the remaining
;;; input.  A pattern variable binds to the whole remaining input (wrapped
;;; as a syntax object so a template can splice it); `_' binds nothing;
;;; anything else is matched as a datum/literal.

(define (pattern-match-tail pat-tail input-form input-stx literals bindings)
  (if (pattern-variable? pat-tail literals)
      (cons (cons (pattern-leaf-datum pat-tail) (datum->syntax input-stx input-form)) bindings)
      (if (and (identifier? pat-tail) (eq? (syntax-form pat-tail) '_))
          bindings
          (pattern-match* pat-tail (datum->syntax input-stx input-form) literals bindings))))

(define (pattern-match-ellipsis elem-pat rest-pat input-form input-stx literals bindings)
  (letrec* ((len (length input-form))
            (rest-min (pattern-min-length rest-pat)))
    (if (< len rest-min)
        #f
        (letrec* ((repeat-count (- len rest-min))
                  (loop (lambda (i inputs accum-bindings)
                          (if (= i repeat-count)
                              (pattern-match-list rest-pat inputs input-stx literals accum-bindings)
                              (letrec* ((elem-input (car inputs))
                                        (elem-bindings (pattern-match-ellipsis-elem elem-pat elem-input literals)))
                                (if (not elem-bindings)
                                    #f
                                    (loop (+ i 1)
                                          (cdr inputs)
                                          (merge-ellipsis-bindings elem-bindings accum-bindings))))))))
          (loop 0 input-form bindings)))))

(define (pattern-min-length pat-list)
  (if (null? pat-list)
      0
      (if (not (pair? pat-list))
          0
          (if (and (pair? (cdr pat-list))
                   (ellipsis-datum? (cadr pat-list)))
              (pattern-min-length (cddr pat-list))
              (+ 1 (pattern-min-length (cdr pat-list)))))))

(define (pattern-match-ellipsis-elem elem-pat input literals)
  (pattern-match* elem-pat input literals '()))

(define (merge-ellipsis-bindings elem-bindings accum)
  (if (null? elem-bindings)
      accum
      (letrec* ((var (caar elem-bindings))
                (val (cdar elem-bindings))
                (existing (assq var accum)))
        (if existing
            (begin
              (set-cdr! existing (append (cdr existing) (list val)))
              (merge-ellipsis-bindings (cdr elem-bindings) accum))
            (merge-ellipsis-bindings (cdr elem-bindings)
                                     (cons (list var val) accum))))))

;;; Template instantiation
;;;
;;; instantiate : syntax bindings -> syntax
;;;
;;; This is the `syntax` (#') template form, implemented as a procedure.
;;; The template is kept as a syntax object: identifiers retain the
;;; definition-site scopes and home library, so free identifiers in the
;;; output resolve at the macro definition site (referential
;;; transparency, cf. core-model reftrans example).  Pattern variables
;;; are replaced with their bound input syntax; the expand-macro flip
;;; mechanism distinguishes introduced from use-site syntax.

(define (ellipsis-stx? x)
  (if (syntax? x) (eq? (syntax-form x) '...) #f))

(define (instantiate template bindings)
  (instantiate* template bindings))

(define (instantiate* template bindings)
  (letrec* ((form (syntax-form template)))
    (if (symbol? form)
        (letrec* ((binding (assq form bindings)))
          (if binding (cdr binding) template))
        (if (pair? form)
            (make-syntax (instantiate-list form bindings)
                         (syntax-context template)
                         (syntax-library template))
            ;; Vector template (e.g. match.scm's #(vec ...)): elements may
            ;; be raw datums (vectors are wrapped whole), so wrap them as
            ;; syntax, instantiate as a list, and rebuild the vector.
            (if (vector? form)
                (letrec* ((elems (map (lambda (e)
                                        (if (syntax? e)
                                            e
                                            (make-syntax e (syntax-context template)
                                                         (syntax-library template))))
                                      (vector->list form))))
                  (make-syntax (list->vector (instantiate-list elems bindings))
                               (syntax-context template)
                               (syntax-library template)))
                template)))))

(define (instantiate-list elems bindings)
  (if (null? elems)
      '()
      ;; Dotted tail: a syntax object wrapping a pattern variable; splice
      ;; its bound value's contents as the improper tail.  A list-valued
      ;; tail splices its elements ((list . x) with x=(1 2 3) -> (list 1 2 3));
      ;; a single-valued tail stays a syntax object so the tree remains
      ;; fully wrapped (Racket keeps every leaf a syntax object) -- unwrapping
      ;; a single symbol here produced a bare `x' that broke stx-flip-scope.
      (if (and (syntax? elems) (symbol? (syntax-form elems)))
          (letrec* ((binding (assq (syntax-form elems) bindings)))
            (if binding
                (letrec* ((v (cdr binding)))
                  (if (and (syntax? v) (list? (syntax-form v)))
                      (syntax-form v)
                      v))
                elems))
          (if (and (pair? (cdr elems))
                   (ellipsis-stx? (cadr elems)))
              (append (instantiate-ellipsis (car elems) bindings)
                      (instantiate-list (cddr elems) bindings))
              (cons (instantiate* (car elems) bindings)
                    (instantiate-list (cdr elems) bindings))))))

(define (instantiate-ellipsis elem-template bindings)
  (letrec* ((vars (template-vars elem-template))
            (len (letrec* ((loop (lambda (vs)
                                   (if (null? vs)
                                       0
                                       (letrec* ((entry (assq (car vs) bindings)))
                                         (if (and entry (list? (cdr entry)))
                                             (length (cdr entry))
                                             (loop (cdr vs))))))))
                    (loop vars)))
            (loop (lambda (i results)
                    (if (= i len)
                        (reverse results)
                        (letrec* ((indexed-bindings (index-bindings vars bindings i)))
                          (loop (+ i 1)
                                (cons (instantiate* elem-template indexed-bindings)
                                      results)))))))
    (loop 0 '())))

(define (template-vars template)
  (letrec* ((form (syntax-form template)))
    (if (eq? form '...)
        '()
        (if (symbol? form)
            (list form)
            (if (pair? form)
                (template-vars-list form)
                '())))))

(define (template-vars-list lst)
  (if (null? lst)
      '()
      (if (syntax? lst)
          (template-vars lst)
          (append (template-vars (car lst))
                  (template-vars-list (cdr lst))))))

(define (index-bindings vars bindings i)
  (letrec* ((loop (lambda (vs result)
                    (if (null? vs)
                        (reverse result)
                        (letrec* ((binding (assq (car vs) bindings)))
                          (if binding
                              (loop (cdr vs)
                                    (cons (cons (car vs) (list-ref (cdr binding) i)) result))
                              (loop (cdr vs) result)))))))
    (loop vars '())))

;;; syntax-case-dispatch : input literals-stx (list clause-spec) -> syntax
;;; Run-time matcher.  Each clause-spec is (list pattern-datum patvars
;;; fender-proc body-proc).  Tries clauses in order: match the input against
;;; the pattern, apply the fender to the matched pattern-variable values, and
;;; if it holds apply the body.  Signals an error if no clause matches.

(define (syntax-case-dispatch input literals-stx clauses)
  (letrec* ((literals (if (syntax? literals-stx) (syntax-form literals-stx) '()))
            (loop (lambda (cls)
                    (if (null? cls)
                        (error "syntax-case: no matching clause"
                               (if (syntax? input) (syntax->datum input) input))
                        (letrec* ((cl (car cls))
                                  (pat (pattern-tree (car cl)))
                                  (patvars (cadr cl))
                                  (fender (caddr cl))
                                  (body (cadddr cl))
                                  (form (if (syntax? input) (syntax-form input) input))
                                  (bindings (if (list? form)
                                                (pattern-match-list pat form input literals '())
                                                (pattern-match* pat input literals '()))))
                          (if bindings
                              (letrec* ((vals (map (lambda (p)
                                                     (letrec* ((e (assq p bindings)))
                                                       (if e (cdr e) '())))
                                                   patvars)))
                                (if (apply fender vals)
                                    (apply body vals)
                                    (loop (cdr cls))))
                              (loop (cdr cls))))))))
    (loop clauses)))