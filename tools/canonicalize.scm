;; canonicalize.scm FILE -> print a gensym-blind copy of FILE.
;;
;; The expander's global fresh counter (*fresh-counter*) makes names like
;; `rtd~493`; how many were consumed before a rebuild depends on the boot
;; chain, so the raw artifact text is NOT comparable across rebuilds.  This
;; tool rewrites every `stem~<digits>` symbol suffix to `stem~?`, leaving
;; everything else untouched; comparing canonical forms checks structural
;; equality of the expansion.  Store-local numbers (`name:N`, per-store and
;; deterministic) are kept as-is.

(import (scheme base) (scheme read) (scheme write) (scheme char)
        (liii base))

(define (canon-symbol s)
  ;; rewrite every '~' + digit run to '~?' -- rtd~493:32 -> rtd~?:32
  (let ((n (string-length s)))
    (let loop ((i 0) (acc '()))
      (if (>= i n)
        (list->string (reverse acc))
        (if (and (char=? (string-ref s i) #\~)
                 (< (+ i 1) n)
                 (char-numeric? (string-ref s (+ i 1))))
          (let skip ((j (+ i 2)))
            (if (and (< j n) (char-numeric? (string-ref s j)))
              (skip (+ j 1))
              (loop j (cons #\? (cons #\~ acc)))))
          (loop (+ i 1) (cons (string-ref s i) acc)))))))

(define (canon x)
  (cond ((symbol? x) (string->symbol (canon-symbol (symbol->string x))))
        ((pair? x) (cons (canon (car x)) (canon (cdr x))))
        (else x)))

(let ((args (command-line)))
  ;; last argument is the file path (gf [options] script.scm)
  (let ((path (list-ref args (- (length args) 1))))
    (call-with-input-file path
      (lambda (p)
        (let loop ()
          (let ((d (read p)))
            (unless (eof-object? d)
              (write (canon d))
              (newline)
              (loop))))))))
