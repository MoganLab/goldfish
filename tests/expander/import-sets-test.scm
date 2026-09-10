(import (liii check) (goldfish) (liii os) (srfi srfi-13))

;; Import-set semantics regression: nested R7RS import sets and
;; Racket-style import conflicts.
;;
;; Depth-1 sets ((only ...), (except ...), (prefix ...), (rename ...) over a
;; library) and bare library imports have always worked; the newer surface
;; is (a) NESTED sets -- a modifier over another modifier -- and (b) an
;; import-time error when two libraries provide the same name with
;; different bindings, unless one side is the implementation substrate.
;; Import errors surface through the expander's runtime entry points
;; (load-library!), so they are assertable in-process.

;; Contract: #f if thunk raised nothing, else the guard's rendered
;; detail when wrapped, otherwise the raw error template head.
(define (import-error-message thunk)
  (catch #t
    thunk
    (lambda (tag . info)
      (if (and (pair? info)
               (pair? (car info))
               (string? (caar info)))
        (let ([payload (car info)])
          (if (and (equal? (car payload) "import: failed to load library ~a: ~a")
                   (= (length payload) 3))
            (caddr payload)
            (car payload)))
        #f))))

;; Substring assertions survive message rewording; a silenced import
;; yields #f (not a string), so a disabled check still fails loudly.
(define (detail-mentions? detail . fragments)
  (and (string? detail)
       (let loop ([fs fragments])
         (or (null? fs)
             (and (string-contains detail (car fs))
                  (loop (cdr fs)))))))

;; ===== 1. Nested import sets compose =====
;; Names are drawn from (liii os) so they are not already ambient in the
;; test mode, isolating the composition mechanics.
(import (only (liii os) mkdir rmdir os-sep))
(check (procedure? mkdir) => #t)
(check (procedure? rmdir) => #t)
(check (char? (os-sep)) => #t)

(import (prefix (only (liii os) os-temp-dir) zz-))
(check (procedure? zz-os-temp-dir) => #t)

(import (rename (prefix (only (liii os) os-temp-dir) yy-) (yy-os-temp-dir ytd)))
(check (procedure? ytd) => #t)

;; only over except: remove car/cdr/list from the subset first.
(import (only (except (scheme base) car cdr) + list))
(check (+ 1 2) => 3)
(check (list 1 2) => '(1 2))

;; ===== 2. Import conflicts are errors (Racket-style) =====
;; Two peer libraries bind the same exported name differently; a library
;; importing both is an error at import time.
;; Per-run root (pid-suffixed) so concurrent `gf test -j` workers and
;; leftover fixture trees never share library files.  The `ct/` leaf is
;; fixed: (ct one) resolves to <root>/ct/one.scm through *load-path*.
(define fixture-dir (string-append (os-temp-dir) "/ct-" (number->string (getpid))))
(define fixture-sub (string-append fixture-dir "/ct"))
(catch #t (lambda () (mkdir fixture-dir)) (lambda args #f))
(catch #t (lambda () (mkdir fixture-sub)) (lambda args #f))
(call-with-output-file (string-append fixture-sub "/one.scm")
  (lambda (p)
    (write '(define-library (ct one)
              (import (scheme base))
              (export ct-name)
              (define ct-name 'from-one))
           p)
    (newline p)))
(call-with-output-file (string-append fixture-sub "/two.scm")
  (lambda (p)
    (write '(define-library (ct two)
              (import (scheme base))
              (export ct-name)
              (define ct-name 'from-two))
           p)
    (newline p)))
(call-with-output-file (string-append fixture-sub "/both.scm")
  (lambda (p)
    (write '(define-library (ct both)
              (import (ct one) (ct two))
              (export x)
              (define x 0))
           p)
    (newline p)))
;; re-export of one's binding under the same name: same binding object.
(call-with-output-file (string-append fixture-sub "/one-reexport.scm")
  (lambda (p)
    (write '(define-library (ct one-reexport)
              (import (ct one))
              (export ct-name))
           p)
    (newline p)))
(and (not (member fixture-dir *load-path*))
     (set! *load-path* (cons fixture-dir *load-path*)))

;; single import is fine
(check (catch #t
         (lambda () (load-library! '(ct one)) 'ok)
         (lambda args 'error))
       => 'ok)
;; importing two conflicting peers is an error mentioning the name
(check (detail-mentions? (import-error-message (lambda () (load-library! '(ct both))))
                             "already imported with a different binding" "ct-name")
       => #t)
;; the same binding reaching a library through a re-export path is fine
(check (catch #t
         (lambda () (load-library! '(ct one-reexport)) 'ok)
         (lambda args 'error))
       => 'ok)

;; ===== 3. Composition edges =====
;; only over a renamed set: the new name is what only selects.
(import (only (rename (liii os) (os-temp-dir tmp-dir-fn)) tmp-dir-fn))
(check (procedure? tmp-dir-fn) => #t)
;; partial rename keeps unrenamed exports under their own names.
(import (rename (liii os) (os-sep slash)))
(check (char? (slash)) => #t)
(check (procedure? mkdir) => #t)
;; prefix of a rename: prefix applies to the name as renamed.
(import (prefix (rename (liii os) (os-sep sep2)) pre-))
(check (char? (pre-sep2)) => #t)
;; depth-3 nesting: prefix over except over only.
(import (prefix (except (only (liii os) mkdir rmdir) rmdir) oo-))
(check (procedure? oo-mkdir) => #t)
;; excepting an id the source does not export is a no-op.
(import (except (liii os) no-such-os-export))
(check (procedure? rmdir) => #t)

;; A rename that collapses two different exports onto one name is an error
;; when the import set is actually applied.
(call-with-output-file (string-append fixture-sub "/dup-rename.scm")
  (lambda (p)
    (write '(define-library (ct dup-rename)
              (import (rename (liii os) (os-sep dx) (mkdir dx)))
              (export dx)
              (define dx 0))
           p)
    (newline p)))
(check (detail-mentions? (import-error-message (lambda () (load-library! '(ct dup-rename))))
                             "bound more than once with different bindings" "dx")
       => #t)

;; ===== 4. R7RS `for' level specs =====
;; (for import-set level ...) chooses the phases an import is visible at.
;; Resolution is phase-blind today, so the levels are accepted and the
;; inner set is imported; the shape is validated (at least one level).
(import (for (liii os) run expand))
(check (procedure? mkdir) => #t)
(import (for (only (liii os) os-temp-dir) expand))
(check (procedure? os-temp-dir) => #t)
(import (for (prefix (liii os) ff-) run))
(check (procedure? ff-mkdir) => #t)
;; a for spec without levels is an error
(call-with-output-file (string-append fixture-sub "/for-nolevel.scm")
  (lambda (p)
    (write '(define-library (ct for-nolevel)
              (import (for (scheme base)))
              (export x)
              (define x 0))
           p)
    (newline p)))
(check (detail-mentions? (import-error-message (lambda () (load-library! '(ct for-nolevel))))
                             "for spec needs an import set and at least one level")
       => #t)

;; ===== 5. R7RS export rename (rename <from> <to>) =====
;; Re-export `from' under the visible name `to': the alias shares `from's
;; binding object, so values, macros (own and imported), and the warm
;; cache restore all resolve it.  Fixtures live under xr/ (same
;; pid-suffixed root as ct/, same *load-path* entry).
(define fixture-xr (string-append fixture-dir "/xr"))
(catch #t (lambda () (mkdir fixture-xr)) (lambda args #f))
(define (write-xr-fixture! leaf datum)
  (call-with-output-file (string-append fixture-xr "/" (symbol->string leaf) ".scm")
    (lambda (p) (write datum p) (newline p))))
(write-xr-fixture! 'vals
  '(define-library (xr vals)
     (import (scheme base))
     (export the-answer (rename the-answer answer2))
     (define the-answer 41)))
(write-xr-fixture! 'macros
  '(define-library (xr macros)
     (import (scheme base))
     (export my-or (rename my-or my-or2))
     (define-syntax my-or
       (syntax-rules () ((my-or e) e) ((my-or e e2 ...) (if e e #f))))))
(write-xr-fixture! 'reexp
  '(define-library (xr reexp)
     (import (xr macros))
     (export (rename my-or2 my-or3))))
(write-xr-fixture! 'muse
  '(define-library (xr muse)
     (import (scheme base) (xr macros))
     (export got2)
     (define got2 (my-or2 7))))
(write-xr-fixture! 'reuse
  '(define-library (xr reuse)
     (import (scheme base) (xr reexp))
     (export got3)
     (define got3 (my-or3 8))))
(write-xr-fixture! 'comp
  '(define-library (xr comp)
     (import (scheme base)
             (only (xr vals) answer2)
             (prefix (xr vals) v-))
     (export gotc)
     (define gotc (list answer2 v-the-answer v-answer2))))
(write-xr-fixture! 'nestok
  '(define-library (xr nestok)
     (import (scheme base)
             (rename (only (xr vals) the-answer answer2)
                     (the-answer x) (answer2 x)))
     (export gotx)
     (define gotx (+ x 1))))
(write-xr-fixture! 'nestdup
  '(define-library (xr nestdup)
     (import (scheme base)
             (rename (only (scheme base) car cdr) (car x) (cdr x)))
     (export gotx)
     (define gotx 0)))
(write-xr-fixture! 'badfrom
  '(define-library (xr badfrom)
     (import (scheme base))
     (export (rename nosuch x))
     (define x 0)))
(write-xr-fixture! 'badtarget
  '(define-library (xr badtarget)
     (import (scheme base))
     (export a b (rename b a))
     (define a 1)
     (define b 2)))

;; %internal-names are ambient primitives in (goldfish)-importing code.
(define (xr-runtime name id)
  (module-ref (lookup-module name) id))
(define (xr-fixture-path leaf)
  (string-append fixture-xr "/" (symbol->string leaf) ".scm"))
;; Direct capture -> restore proof (independent of cache-dir state):
;; rebuild a fixture from its captured record and resolve the alias in
;; the rebuilt library.  Value aliases ride the bindings section, but a
;; macro alias is absent from the macro replay section -- without the
;; record's rename specs the restore's export check fails.
;; NOTE: do NOT assert on .gfo files here -- the test runner sets
;; GOLDFISH_CACHE_READONLY=1 (goldtest.scm), so fixture loads never
;; write entries; this direct rebuild is the warm-restore proof.
(define (xr-restore-ref leaf id)
  (let ((forms (call-with-input-file (xr-fixture-path leaf) read-forms)))
    (call-with-values
      (lambda () (capture-file-cache forms))
      (lambda (recs ctx)
        (exp-library-ref (restore-library-cache (car recs)) id)))))

;; value alias: visible under both names, cold and warm
(check (catch #t (lambda () (load-library! '(xr vals)) 'ok) (lambda args 'error))
       => 'ok)
(check (if (memq 'answer2 (lib-record-exports (library-registry-ref '(xr vals)))) #t #f)
       => #t)
(check (xr-runtime '(xr vals) 'answer2) => 41)
(check (xr-runtime '(xr vals) 'the-answer) => 41)
(check (toplevel-binding? (xr-restore-ref 'vals 'answer2)) => #t)
;; second load restores from the cache entry: the alias must survive
(check (catch #t (lambda () (load-library! '(xr vals)) 'ok) (lambda args 'error))
       => 'ok)
(check (xr-runtime '(xr vals) 'answer2) => 41)

;; own-macro alias, used through an importer, cold and warm
(check (catch #t (lambda () (load-library! '(xr muse)) 'ok) (lambda args 'error))
       => 'ok)
(check (xr-runtime '(xr muse) 'got2) => 7)
(check (transformer-binding? (xr-restore-ref 'macros 'my-or2)) => #t)
(check (catch #t (lambda () (load-library! '(xr muse)) 'ok) (lambda args 'error))
       => 'ok)
(check (xr-runtime '(xr muse) 'got2) => 7)

;; imported-macro alias: `from' exists only via the restored re-import,
;; so the rebuild exercises the record's rename specs
(check (catch #t (lambda () (load-library! '(xr reuse)) 'ok) (lambda args 'error))
       => 'ok)
(check (xr-runtime '(xr reuse) 'got3) => 8)
(check (transformer-binding? (xr-restore-ref 'reexp 'my-or3)) => #t)
(check (catch #t (lambda () (load-library! '(xr reuse)) 'ok) (lambda args 'error))
       => 'ok)
(check (xr-runtime '(xr reuse) 'got3) => 8)

;; import sets compose over aliases
(check (catch #t (lambda () (load-library! '(xr comp)) 'ok) (lambda args 'error))
       => 'ok)
(check (xr-runtime '(xr comp) 'gotc) => '(41 41 41))

;; nested rename over an export alias: same binding collapsing onto one
;; name is fine (import-view arbitrates both paths uniformly)
(check (catch #t (lambda () (load-library! '(xr nestok)) 'ok) (lambda args 'error))
       => 'ok)
(check (xr-runtime '(xr nestok) 'gotx) => 42)
;; nested rename collapsing two different bindings still errors
(check (detail-mentions? (import-error-message (lambda () (load-library! '(xr nestdup))))
                             "bound more than once with different bindings" "x")
       => #t)
;; error cases: unknown source, colliding target
(check (detail-mentions? (import-error-message (lambda () (load-library! '(xr badfrom))))
                             "export has no binding" "nosuch")
       => #t)
(check (detail-mentions? (import-error-message (lambda () (load-library! '(xr badtarget))))
                             "already bound" "a")
       => #t)

(check-report)
