(import (scheme base) (scheme char) (scheme eval) (scheme write) (liii check))
(check-set-mode! 'report-failed)
(include "../fixtures/semantic-audit.scm")
(audit-check 'data.disjoint (lambda () (list (number? #\a) (char? 97) (string? 'a) (symbol? "a")
                                          (vector? (bytevector 1)) (bytevector? (vector 1)) (pair? '())
                                          (boolean? 0) (procedure? (open-input-string "")))) '(#f #f #f #f #f #f #f #f #f))
(audit-check 'data.truth (lambda () (list (if '() #t #f) (if 0 #t #f) (if "" #t #f) (not #f) (not '()) (boolean=? #t #t #t)))
  '(#t #t #t #t #f #t))
(audit-check 'data.pair-mutation
  (lambda () (let* ((x (cons 1 2)) (alias x)) (set-car! x 3) (set-cdr! alias 4) (list (eq? x alias) x))) '(#t (3 . 4)))
(audit-check 'data.list-copy
  (lambda () (let* ((x (cons 1 (cons 2 3))) (y (list-copy x)))
               (list (equal? x y) (eq? x y) (eq? (cdr x) (cdr y))))) '(#t #f #f))
(audit-check 'data.append-tail
  (lambda () (let* ((tail (list 3 4)) (x (append (list 1 2) tail))) (list x (eq? (cddr x) tail)))) '((1 2 3 4) #t))
(audit-check 'data.list-tail
  (lambda () (let ((x (list 1 2 3))) (list (list-tail x 2) (eq? (list-tail x 0) x)))) '((3) #t))
(audit-check 'data.membership
  (lambda () (let ((x (list 1 2 3)))
               (list (eq? (member 2 x) (cdr x)) (member 12 x (lambda (a b) (= (- a 10) b)))
                     (assoc 12 '((1 . a) (2 . b)) (lambda (a b) (= (- a 10) b)))))) '(#t (2 3) (2 . b)))
(audit-check 'data.symbols (lambda () (list (symbol=? 'λ (string->symbol "λ"))
                                         (symbol->string (string->symbol "two words"))
                                         (symbol=? 'A 'a))) '(#t "two words" #f))
(audit-check 'data.characters (lambda () (list (char->integer #\λ) (integer->char 128578)
                                             (char<? #\a #\b #\c) (char-ci=? #\Σ #\σ))) '(955 #\🙂 #t #t))
(audit-check 'data.unicode-properties (lambda () (list (char-alphabetic? #\λ) (char-whitespace? #\x2003)
                                                      (digit-value #\x0664) (char-numeric? #\x0664))) '(#t #t 4 #t))
(audit-check 'data.string-index
  (lambda () (let ((s (string-copy "aλ🙂b"))) (string-set! s 1 #\中)
               (list (string-length s) (string-ref s 2) (substring s 1 3) (string->list s 1 3))))
  '(4 #\🙂 "中🙂" (#\中 #\🙂)))
(audit-check 'data.string-overlap
  (lambda () (let ((s (string-copy "aλ🙂b"))) (string-copy! s 1 s 0 3) s)) "aaλ🙂")
(audit-check 'data.string-traversal
  (lambda () (let ((seen '())) (string-for-each (lambda (c) (set! seen (cons c seen))) "λ🙂")
               (list (reverse seen) (string-map (lambda (a b) b) "abc" "λ🙂")))) '((#\λ #\🙂) "λ🙂"))
(audit-check 'data.string-case
  (lambda () (list (string-foldcase "Straße") (string-ci=? "STRASSE" "Straße") (string-upcase "λ")))
  '("strasse" #t "Λ"))
(audit-check 'data.vector-overlap
  (lambda () (let ((v (vector 1 2 3 4))) (vector-copy! v 1 v 0 3) v)) '#(1 1 2 3))
(audit-check 'data.vector-ranges
  (lambda () (let ((v (vector 1 2 3 4))) (vector-fill! v 9 1 3)
               (list v (vector-copy v 1 3) (vector->list v 1 3)))) '(#(1 9 9 4) #(9 9) (9 9)))
(audit-check 'data.vector-string
  (lambda () (list (vector->string '#(#\a #\λ #\🙂) 1 3) (string->vector "aλ🙂" 1 3))) '("λ🙂" #(#\λ #\🙂)))
(audit-check 'data.bytevector-overlap
  (lambda () (let ((v (bytevector 0 1 2 255))) (bytevector-copy! v 1 v 0 3) v)) (bytevector 0 0 1 2))
(audit-check 'data.bytevector-ranges
  (lambda () (list (bytevector-copy (bytevector 0 1 2 255) 1 3)
                   (bytevector-append (bytevector 0) (bytevector) (bytevector 255))))
  (list (bytevector 1 2) (bytevector 0 255)))
(audit-check 'data.utf8-ranges
  (lambda () (list (string->utf8 "aλ🙂b" 1 3) (utf8->string (string->utf8 "aλ🙂b") 1 7)))
  (list (bytevector 206 187 240 159 153 130) "λ🙂"))
(audit-check 'data.fresh-storage
  (lambda () (let ((a (make-vector 1 0)) (b (make-vector 1 0)) (s (make-string 1 #\a)) (t (make-string 1 #\a)))
               (vector-set! a 0 1) (string-set! s 0 #\b) (list (eqv? a b) b (eqv? s t) t))) '(#f #(0) #f "a"))
(check-report)
