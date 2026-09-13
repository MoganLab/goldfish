;; Persistent test worker: run many test files in one process, amortizing
;; boot plus library-restore costs across the chunk. Each file still gets
;; a fresh program library (reset-program-library!) and fresh check
;; counters (check-reset!); -m seed imports are replayed from the worker's
;; own boot environment. Files must be independent (no cross-file
;; top-level state assumptions); failures and errors are contained per
;; file and reported as machine-readable summary lines.
;;
;; Usage: gf -m liii tools/test/liii/worker.scm -- <file> ...
;; Requires GOLDFISH_CHECK_NO_EXIT=1 so a failing file's check-report
;; records instead of exiting the worker.
;;
;; Output per file (stdout, alongside the file's own output):
;;   ;;;WORKER <file> <code> <ms>      ; code: 0 pass, 1 failed checks, 2 error

(import (goldfish) (liii os) (srfi srfi-19) (srfi srfi-78)
        (liii subprocess))

(define (now-ms)
  (let ((t (current-time)))
    (+ (* (time-second t) 1000)
       (quotient (time-nanosecond t) 1000000))))

;;; -m seed imports, replayed per file (import order = reverse of uses,
;;; minus this worker's own imports, which must not leak into test files).
;;; (goldfish) stays: it is both a -m seed and a worker import, and files
;;; need the seed copy.
(define worker-own-imports '((liii os) (srfi srfi-19) (srfi srfi-78)))

(define seed-names
  (let ((all (reverse (map (lambda (u) (exp-library-name (car u)))
                           (exp-library-uses (program-library))))))
    (let loop ((ns all) (acc '()))
      (if (null? ns)
        (reverse acc)
        (loop (cdr ns)
              (if (member (car ns) worker-own-imports)
                acc
                (cons (car ns) acc)))))))

(define seed-counter 0)

(define (write-seed-file!)
  ;; Unique seed per file (deleted after load): never pollute the shared
  ;; temp dir, and never hit a stale seed artifact.
  (set! seed-counter (+ seed-counter 1))
  (let ((f (string-append (os-temp-dir) "/gf-worker-seed-"
                          (number->string (getpid)) "-"
                          (number->string seed-counter) ".scm")))
    (call-with-output-file f
      (lambda (p)
        (display "(import" p)
        (for-each (lambda (n) (display " " p) (write n p)) seed-names)
        (display ")" p)
        (newline p)))
    f))

(define reset-program-library!
  (module-ref the-expander-library 'reset-program-library!))

(define (clear-artifact! src)
  ;; Drop this file's program artifacts (all opt levels).
  (let ((base (string-append (gfo-dir) "/" (gfo-key src))))
    (for-each (lambda (suffix)
                (let ((f (string-append base suffix ".gfo")))
                  (when (file-exists? f) (delete-file f))))
              '("" "-o1" "-o2" "-o3"))))

(define (seeded-ok?)
  ;; The seed import must make substrate names (e.g. expand-eval)
  ;; resolvable in the fresh program library.
  (and (exp-library-ref-at-phase (program-library) 'expand-eval 0) #t))

(define (run-one f)
  (let ((t0 (now-ms)))
    (reset-program-library!)
    (check-reset!)
    (run-reset!)
    (let ((code (catch #t
                  (lambda ()
                    (let ((seed (write-seed-file!)))
                      (clear-artifact! seed)
                      (load seed)
                      (delete-file seed)
                      (clear-artifact! seed)
                      (if (not (seeded-ok?))
                      (begin (display ";;;WORKER-SEED-FAIL ")
                             (display f)
                             (newline)
                             2)
                      (begin (load f)
                             (if (check-failed?) 1 0)))))
                  (lambda args
                    (display ";;;WORKER-ERROR ")
                    (display f)
                    (display " ")
                    (write args)
                    (newline)
                    2))))
      (let ((ms (- (now-ms) t0)))
        (display ";;;WORKER ")
        (display f)
        (display " ")
        (display code)
        (display " ")
        (display ms)
        (newline)
        code))))

(define (files-after-dd args)
  (let loop ((as (cdr args)))
    (cond ((null? as) '())
          ((equal? (car as) "--") (cdr as))
          (else (loop (cdr as))))))

(let ((files (files-after-dd (command-line)))
      (failed 0))
  (for-each (lambda (f)
              (let ((code (run-one f)))
                (if (not (zero? code))
                  (set! failed (+ failed 1)))))
            files)
  (display ";;;WORKER-DONE ")
  (display (length files))
  (display " ")
  (display failed)
  (newline))
