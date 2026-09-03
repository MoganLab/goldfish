(import (liii check) (goldfish) (liii os))

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

(define (import-error-message thunk)
  (catch #t
    thunk
    (lambda (tag . info)
      (if (and (pair? info)
               (pair? (car info))
               (string? (caar info)))
        (caar info)
        #f))))

;; ===== 1. Nested import sets compose =====
;; Names are drawn from (liii os) so they are not already ambient in the
;; test mode, isolating the composition mechanics.
(import (only (liii os) mkdir rmdir os-sep))
(check (procedure? mkdir) => #t)
(check (procedure? rmdir) => #t)
(check (string? os-sep) => #t)

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
(define fixture-dir (os-temp-dir))
(define fixture-sub (string-append fixture-dir "/ct"))
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
(if (not (member fixture-dir *load-path*))
  (set! *load-path* (cons fixture-dir *load-path*)))

;; single import is fine
(check (catch #t
         (lambda () (load-library! '(ct one)) 'ok)
         (lambda args 'error))
       => 'ok)
;; importing two conflicting peers is an error mentioning the name
(check (import-error-message (lambda () (load-library! '(ct both))))
       => "import: ct-name already imported with a different binding")
;; the same binding reaching a library through a re-export path is fine
(check (catch #t
         (lambda () (load-library! '(ct one-reexport)) 'ok)
         (lambda args 'error))
       => 'ok)
