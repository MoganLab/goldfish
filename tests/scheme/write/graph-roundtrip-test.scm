(import (scheme base) (scheme read) (scheme write) (liii check))
(check-set-mode! 'report-failed)

(define (printed writer obj)
  (let ((port (open-output-string)))
    (writer obj port)
    (get-output-string port)))

(define (restored writer obj)
  (read (open-input-string (printed writer obj))))

;; Ordinary write duplicates acyclic aliases; write-shared preserves them.
(let* ((tail (list 'a 'b)) (obj (list tail tail)))
  (check (printed write obj) => "((a b) (a b))")
  (check (printed write-simple obj) => "((a b) (a b))")
  (check (printed write-shared obj) => "(#0=(a b) #0#)")
  (let ((copy (restored write-shared obj)))
    (check (eq? (car copy) (cadr copy)) => #t)
    (set-car! (car copy) 'changed)
    (check (car (cadr copy)) => 'changed)))

;; Shared cdrs must survive dotted-label output.
(let* ((tail (list 2 3)) (obj (list (cons 1 tail) tail))
       (copy (restored write-shared obj)))
  (check (eq? (cdr (car copy)) (cadr copy)) => #t))

(let ((obj (cons 'a '())))
  (set-cdr! obj obj)
  (check (printed write obj) => "#0=(a . #0#)")
  (for-each (lambda (writer)
              (let ((copy (restored writer obj)))
                (check (eq? copy (cdr copy)) => #t)
                (check (car copy) => 'a)))
            (list write write-shared))
  (check (printed display obj) => "#0=(a . #0#)")
  (check-catch 'write-simple (printed write-simple obj)))

(let ((obj (cons #f '())))
  (set-car! obj obj)
  (let ((copy (restored write obj)))
    (check (eq? copy (car copy)) => #t)))

(let* ((pair (cons 'leaf #f)) (vec (vector pair pair)))
  (set-cdr! pair vec)
  (for-each (lambda (writer)
              (let ((copy (restored writer vec)))
                (check (eq? (vector-ref copy 0) (vector-ref copy 1)) => #t)
                (check (eq? copy (cdr (vector-ref copy 0))) => #t)))
            (list write write-shared)))

(let ((obj (vector #f)))
  (vector-set! obj 0 obj)
  (let ((copy (restored write obj)))
    (check (eq? copy (vector-ref copy 0)) => #t)))

;; A small DAG has exponentially many paths; visit each shared node once.
(let* ((obj (let loop ((n 24) (v 'leaf))
              (if (zero? n) v (loop (- n 1) (vector v v)))))
       (copy (restored write-shared obj)))
  (let loop ((n 24) (v copy))
    (if (zero? n)
      (check v => 'leaf)
      (begin
        (check (eq? (vector-ref v 0) (vector-ref v 1)) => #t)
        (loop (- n 1) (vector-ref v 0))))))

;; Mutable atomic leaves also retain aliases under write-shared.
(for-each
  (lambda (leaf)
    (let ((copy (restored write-shared (list leaf leaf))))
      (check (eq? (car copy) (cadr copy)) => #t)))
  (list (string-copy "中文") (bytevector 0 127 255)))

;; Symbols resembling other tokens, delimiters, and escapes round-trip.
(for-each
  (lambda (name)
    (let ((sym (string->symbol name)))
      (for-each (lambda (writer)
                  (check (symbol->string (restored writer sym)) => name))
                (list write write-shared write-simple))))
  (list "" "." "123" "1/2" "-0.0" "+inf.0" "+i" "123abc" "+1abc" ".1abc"
        "#t" "#u8()" "#0#" "a b" "a|b" "a\\b" "a;b" "a'b"
        "中文🙂" "é" "a\nb" "a\tb"
        (string #\a (integer->char 0) (integer->char 27) #\b)))
(check (printed write (string->symbol "中文")) => "|中文|")
(check (printed write (string->symbol "123")) => "|123|")
(check (printed write (string->symbol "a|b\\c")) => "|a\\|b\\\\c|")
(check (printed write 'hello-world) => "hello-world")
(check (printed write '+) => "+")

(let ((text (string #\中 #\🙂 #\" #\\ #\newline
                    (integer->char 0) (integer->char 27))))
  (check (restored write text) => text))
(check (restored write (bytevector 0 128 255)) => (bytevector 0 128 255))
(for-each (lambda (ch) (check (restored write ch) => ch))
          (list #\中 #\🙂 #\space #\newline #\) #\| #\\ (integer->char 0)))
(check (printed display (list "中文" #\🙂 (string->symbol "a b")))
       => "(中文 🙂 a b)")
(check (printed display #\newline) => "\n")
(check (printed write (string (integer->char 0))) => "\"\\x00;\"")
(check-catch 'wrong-number-of-args (write 'a (open-output-string) (open-output-string)))

;; Depth must not truncate readable output to an opaque placeholder.
(let* ((obj (let loop ((n 260) (v 'leaf))
              (if (zero? n) v (loop (- n 1) (list v)))))
       (copy (restored write obj)))
  (check (let loop ((n 260) (v copy))
           (if (zero? n) v (loop (- n 1) (car v)))) => 'leaf))

;; Datum labels belong to each read, including adjacent serialized graphs.
(let* ((leaf (list 1)) (obj (list leaf leaf)) (port (open-output-string)))
  (write-shared obj port)
  (newline port)
  (write-shared obj port)
  (let* ((input (open-input-string (get-output-string port)))
         (first (read input)) (second (read input)))
    (check (eq? (car first) (cadr first)) => #t)
    (check (eq? (car second) (cadr second)) => #t)
    (check (eq? (car first) (car second)) => #f)))

(check-report)
