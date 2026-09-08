;; scan-parens.scm FILE ... -> exit 0 iff every FILE has balanced parens.
;;
;; Masks strings, character literals, block comments (#| |#) and line
;; comments while counting, then reports the line of every surplus close
;; paren and the final depth (unclosed opens).  Line numbers are exactly
;; what a reader error like "unexpected close paren (at byte N)" cannot
;; give you; run this before build-kernel.sh blames the artifact.
;;
;; Usage: ./bin/gf -m liii tools/scan-parens.scm FILE ...

(import (scheme base) (scheme char) (scheme file) (scheme write)
        (liii base))

(define dq (integer->char 34))
(define bs (integer->char 92))
(define pipe (integer->char 124))

;; read the whole file as one string (R7RS-portable get-string-all)
(define (slurp path)
  (call-with-input-file path
    (lambda (p)
      (let loop ((acc '()))
        (let ((c (read-char p)))
          (if (eof-object? c)
              (list->string (reverse acc))
              (loop (cons c acc))))))))

(define (scan path)
  (define src (slurp path))
  (define n (string-length src))
  (define ok #t)
  (let loop ((i 0) (depth 0) (line 1))
    (if (>= i n)
        (begin
          (unless (= depth 0)
            (set! ok #f)
            (display path) (display ": final depth ")
            (display depth) (display " (unclosed open paren)") (newline))
          ok)
        (let ((c (string-ref src i)))
          (cond
           ((char=? c #\newline)
            (loop (+ i 1) depth (+ line 1)))
           ((char=? c dq)
            (let skip ((j (+ i 1)))
              (cond ((>= j n) (loop n depth line))
                    ((char=? (string-ref src j) bs) (skip (+ j 2)))
                    ((char=? (string-ref src j) dq) (loop (+ j 1) depth line))
                    (else (skip (+ j 1))))))
           ((and (char=? c #\#) (< (+ i 1) n)
                 (char=? (string-ref src (+ i 1)) bs))
            ;; #\x literal: skip #\ plus the multi-char name
            (let skip ((j (+ i 2)))
              (if (and (< j n)
                       (or (char-alphabetic? (string-ref src j))
                           (char-numeric? (string-ref src j))
                           (char=? (string-ref src j) #\-)))
                  (skip (+ j 1))
                  (loop j depth line))))
           ((and (char=? c #\#) (< (+ i 1) n)
                 (char=? (string-ref src (+ i 1)) pipe))
            ;; #|...|# block comment
            (let skip ((j (+ i 2)) (ln line))
              (cond ((>= (+ j 1) n) (loop n depth ln))
                    ((and (char=? (string-ref src j) pipe)
                          (char=? (string-ref src (+ j 1)) #\#))
                     (loop (+ j 2) depth ln))
                    ((char=? (string-ref src j) #\newline)
                     (skip (+ j 1) (+ ln 1)))
                    (else (skip (+ j 1) ln)))))
           ((char=? c #\;)
            ;; line comment: skip to end of line
            (let skip ((j i))
              (if (or (>= j n) (char=? (string-ref src j) #\newline))
                  (loop j depth line)
                  (skip (+ j 1)))))
           ((char=? c #\()
            (loop (+ i 1) (+ depth 1) line))
           ((char=? c #\))
            (if (<= depth 0)
                (begin
                  (set! ok #f)
                  (display path) (display ":") (display line)
                  (display ": extra close paren") (newline)
                  (loop (+ i 1) 0 line))
                (loop (+ i 1) (- depth 1) line)))
           (else (loop (+ i 1) depth line)))))))

(define (script-args args)
  ;; command-line looks like (-m MOD script.scm FILE ...) or
  ;; (script.scm FILE ...): drop the flag pair, then the script path.
  (if (and (pair? args) (string=? (car args) "-m"))
      (list-tail args 3)
      (cdr args)))

(define (main args)
  (let ((files (script-args args)))
    (if (null? files)
        (begin (display "usage: scan-parens.scm FILE ...") (newline) (exit 2))
        (let ((all-ok (fold (lambda (acc f) (let ((r (scan f))) (and r acc))) #t files)))
          (exit (if all-ok 0 1))))))

(main (cdr (command-line)))
