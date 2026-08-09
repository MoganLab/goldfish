(load "liii/reader.scm")
(import (liii check))

(check-set-mode! 'report-failed)


;; -----------------------------------------------------------------------------
;; R7RS-small reader coverage tests
;;
;; Scope: Revised^7 Report chapter 2 (lexical conventions), 6.2.5 (number
;; literal syntax), 7.1.1 (formal syntax), 7.1.2 (datum syntax), 2.4 (datum
;; labels).
;;
;; Conventions:
;; - All malformed-input cases assert 'read-error is raised (matching the
;;   built-in read of (scheme read)). The reader implementation should raise
;;   errors via (error 'read-error "...").
;; - The current reader implementation is incomplete, so some cases below
;;   fail until the reader is implemented. The tests define the target
;;   behavior.
;; -----------------------------------------------------------------------------

(define (read-str expr)
  (call-with-input-string expr read))

;; Read all datums from a string (for directives, comments between tokens, ...).
(define (read-all str)
  (let ((port (open-input-string str)))
    (let loop ((d (read port)) (acc '()))
      (if (eof-object? d)
        (reverse acc)
        (loop (read port) (cons d acc))))))

;; =============================================================================
;; 2.3 / 6.3  Booleans
;; =============================================================================

(check (read-str "#t") => #t)
(check (read-str "#f") => #f)
(check (read-str "#true") => #t)
(check (read-str "#false") => #f)

;; =============================================================================
;; 6.2.5  Numbers
;; =============================================================================

;; --- Integers ---
(check (read-str "0") => 0)
(check (read-str "42") => 42)
(check (read-str "-100") => -100)
(check (read-str "+7") => 7)
(check (read-str "007") => 7)
(check (exact? (read-str "42")) => #t)

;; --- Radix prefixes (case-insensitive) ---
(check (read-str "#b101010") => 42)
(check (read-str "#B101010") => 42)
(check (read-str "#o52") => 42)
(check (read-str "#O52") => 42)
(check (read-str "#d42") => 42)
(check (read-str "#D42") => 42)
(check (read-str "#x2A") => 42)
(check (read-str "#X2A") => 42)
(check (read-str "#x2a") => 42)
(check (read-str "#x-2A") => -42)
(check (read-str "#o-52") => -42)
(check (read-str "#b-101") => -5)
;; hex bodies beginning with a prefix letter (b/d/e/i/o/x) must read as digits
(check (read-str "#xbf") => 191)
(check (read-str "#xd8") => 216)
(check (read-str "#xef") => 239)
(check (read-str "#x1e2") => 482)
(check (read-str "#xbeef") => #xbeef)
(check (read-str "#x7f") => 127)
(check (read-str "#x80") => 128)

;; --- Exactness prefixes ---
(check (read-str "#e42") => 42)
(check (exact? (read-str "#e42")) => #t)
(check (read-str "#i42") => 42.0)
(check (inexact? (read-str "#i42")) => #t)
(check (read-str "#e42.0") => 42)
(check (exact? (read-str "#e42.0")) => #t)
(check (read-str "#i-42") => -42.0)

;; Exactness prefix may appear before or after the radix prefix
;; (<prefix> = <radix> <exactness> | <exactness> <radix>)
(check (read-str "#e#x10") => 16)
(check (read-str "#x#e10") => 16)
(check (read-str "#e#b101") => 5)
(check (read-str "#e#d42") => 42)
(check (read-str "#i#x10") => 16.0)
(check (inexact? (read-str "#i#x10")) => #t)

;; --- Rationals ---
(check (read-str "22/7") => 22/7)
(check (read-str "-1/2") => -1/2)
(check (read-str "+1/2") => 1/2)
(check (exact? (read-str "22/7")) => #t)
(check (read-str "#x1/2") => 1/2)
(check (read-str "#e6/10") => 3/5)

;; --- Reals (leading/trailing decimal point) ---
(check (read-str "3.14159") => 3.14159)
(check (read-str "-0.5") => -0.5)
(check (read-str "3.") => 3.0)
(check (read-str ".5") => 0.5)
(check (read-str "5.0") => 5.0)
(check (inexact? (read-str "3.")) => #t)
(check (inexact? (read-str ".5")) => #t)

;; --- Exponents ---
(check (read-str "1e10") => 1e10)
(check (read-str "1E10") => 1e10)
(check (read-str "1e+10") => 1e10)
(check (read-str "2.5e-3") => 0.0025)
(check (read-str "1.5E-3") => 0.0015)
(check (inexact? (read-str "1e10")) => #t)
(check (exact? (read-str "#e1e2")) => #t)

;; --- Infinities and NaN (-nan.0 is also legal) ---
(check (read-str "+inf.0") => +inf.0)
(check (read-str "-inf.0") => -inf.0)
(check (nan? (read-str "+nan.0")) => #t)
(check (nan? (read-str "-nan.0")) => #t)
(check (inexact? (read-str "+inf.0")) => #t)

;; --- Complex numbers: rectangular (real part may be omitted) ---
(check (= (read-str "3+4i") 3+4i) => #t)
(check (= (read-str "1-1i") 1-1i) => #t)
(check (= (read-str "0+2.5i") 0+2.5i) => #t)
(check (= (read-str "+i") (make-rectangular 0 1)) => #t)
(check (= (read-str "-i") (make-rectangular 0 -1)) => #t)
(check (= (read-str "+2i") (make-rectangular 0 2)) => #t)
(check (= (read-str "-2i") (make-rectangular 0 -2)) => #t)
(check (= (read-str "2+i") (make-rectangular 2 1)) => #t)
(check (= (read-str "2-i") (make-rectangular 2 -1)) => #t)
(check (exact? (read-str "3+4i")) => #f) ; S7 keeps non-real numbers inexact (R7RS 6.2.3)
(check (inexact? (read-str "0+2.5i")) => #t)

;; --- Complex numbers: polar r@theta ---
(check (= (read-str "1@2") (make-polar 1 2)) => #t)
(check (= (read-str "1.5@-3") (make-polar 1.5 -3)) => #t)

;; =============================================================================
;; 6.6  Characters
;; =============================================================================

;; All nine named characters
(check (read-str "#\\a") => #\a)
(check (read-str "#\\A") => #\A)
(check (read-str "#\\space") => #\space)
(check (read-str "#\\newline") => #\newline)
(check (read-str "#\\alarm") => #\alarm)
(check (read-str "#\\backspace") => #\backspace)
(check (read-str "#\\delete") => #\delete)
(check (read-str "#\\escape") => #\escape)
(check (read-str "#\\null") => #\null)
(check (read-str "#\\return") => #\return)
(check (read-str "#\\tab") => #\tab)

;; #\ followed by any single character
(check (read-str "#\\(") => #\()
(check (read-str "#\\#") => #\#)
(check (read-str "#\\ ") => #\space)

;; Hex escapes (case-insensitive)
(check (read-str "#\\x61") => #\a)
(check (read-str "#\\x4a") => #\J)
(check (read-str "#\\x4A") => #\J)
(check (read-str "#\\x3bb") => (integer->char #x3bb))

;; Raw non-ASCII character literals (UTF-8 decoded from the byte port)
(check (read-str "#\\λ") => (integer->char #x3bb))
(check (read-str "#\\中") => (integer->char #x4e2d))
(check (read-str "#\\x3bbx") => (integer->char #x3bb))

;; =============================================================================
;; 6.7  Strings
;; =============================================================================

(check (read-str "\"hello\"") => "hello")

;; Defined escapes: \a \b \t \n \r \" \\ \| and \x...;
(check (read-str "\"\\a\"") => (string #\alarm))
(check (read-str "\"\\b\"") => (string #\backspace))
(check (read-str "\"\\t\"") => (string #\tab))
(check (read-str "\"\\n\"") => (string #\newline))
(check (read-str "\"\\r\"") => (string #\return))
(check (read-str "\"Quote: \\\"Escaped\\\"\"") => "Quote: \"Escaped\"")
(check (read-str "\"Backslash: \\\\\"") => "Backslash: \\")
(check (read-str "\"vertical: \\|\"") => "vertical: |")
(check (read-str "\"Hex: \\x61;\"") => "Hex: a")
(check (read-str "\"\\x41;\"") => "A")
(check (read-str "\"\\x3bb;\"") => "λ")

;; Line continuation: backslash + line ending, followed by intraline
;; whitespace which is also ignored
(check (read-str "\"hello \\\n   world\"") => "hello world")
(check (read-str "\"a\\\n b\"") => "ab")

;; An unescaped line ending in a string is equivalent to \n
(check (read-str "\"a\nb\"") => "a\nb")

;; Comment markers inside strings are literal
(check (read-str "\"#| not a comment |#\"") => "#| not a comment |#")
(check (read-str "\"; not a comment\"") => "; not a comment")

;; =============================================================================
;; 2.1 / 6.5  Symbols
;; =============================================================================

(check (read-str "foo") => 'foo)
(check (read-str "lambda") => 'lambda)
(check (read-str "string->number") => 'string->number)

;; Peculiar identifiers
(check (read-str "+") => '+)
(check (read-str "-") => '-)
(check (read-str "...") => '...)
(check (read-str "..") => '..)
(check (read-str "->string") => '->string)
(check (read-str "+soup+") => '+soup+)
(check (read-str "<=?") => '<=?)
(check (read-str ".foo") => '.foo)

;; Special initial characters ! $ % & * / : < = > ? @ ^ _ ~
(check (read-str "!") => '!)
(check (read-str "$x") => '$x)
(check (read-str "%") => '%)
(check (read-str "&") => '&)
(check (read-str "*") => '*)
(check (read-str "/") => '/)
(check (read-str ":") => ':)
(check (read-str "<") => '<)
(check (read-str ">") => '>)
(check (read-str "=") => '=)
(check (read-str "?") => '?)
(check (read-str "@") => '@)
(check (read-str "^") => '^)
(check (read-str "_") => '_)
(check (read-str "~") => '~)

;; Case sensitivity
(check (read-str "Foo") => 'Foo)
(check (eq? (read-str "Foo") (read-str "foo")) => #f)

;; |...| escaped identifiers
(check (read-str "|hello world|") => (string->symbol "hello world"))
(check (read-str "|foo\\|bar|") => (string->symbol "foo|bar"))
(check (read-str "|H\\x65;llo|") => 'Hello)
(check (read-str "|\\t\\t|") => (read-str "|\\x9;\\x9;|"))
;; || is a valid R7RS identifier, but the host (S7) cannot represent an
;; empty symbol (string->symbol of "" raises), so it is not tested here.

;; =============================================================================
;; 6.4  Lists & Pairs
;; =============================================================================

(check (read-str "()") => '())
(check (read-str "(1 2 3)") => '(1 2 3))
(check (read-str "(a . b)") => '(a . b))
(check (read-str "(1 2 . 3)") => '(1 2 . 3))
(check (read-str "(1 . (2 3))") => '(1 2 3))
(check (read-str "((a) (b c))") => '((a) (b c)))
(check (read-str "(a (b (c)))") => '(a (b (c))))

;; =============================================================================
;; 6.8  Vectors
;; =============================================================================

(check (read-str "#()") => '#())
(check (read-str "#(1 2 3)") => '#(1 2 3))
(check (read-str "#(a \"hello\" 42)") => '#(a "hello" 42))
(check (read-str "#(#(1) #(2))") => '#(#(1) #(2)))
(check (read-str "#(1 'a)") => '#(1 (quote a)))

;; =============================================================================
;; 6.9  Bytevectors
;; =============================================================================

(check (read-str "#u8()") => #u8())
(check (read-str "#u8(0 127 255)") => #u8(0 127 255))
(check (read-str "#u8(255 128 0)") => #u8(255 128 0))

;; =============================================================================
;; 4.1.2 / 4.2.8  Quote and abbreviations
;; =============================================================================

(check (read-str "'x") => '(quote x))
(check (read-str "`x") => '(quasiquote x))
(check (read-str ",x") => '(unquote x))
(check (read-str ",@x") => '(unquote-splicing x))
(check (read-str "''x") => '(quote (quote x)))
(check (read-str "'#(1 2)") => '(quote #(1 2)))
(check (read-str "`(a ,(b ,@c))")
       => '(quasiquote (a (unquote (b (unquote-splicing c))))))

;; =============================================================================
;; 2.2  Comments
;; =============================================================================

;; Line comment
(check (read-str ";; comment\n 42") => 42)

;; Block comment (nesting supported)
(check (read-str "#| block comment |# 42") => 42)
(check (read-str "#| outer #| inner |# outer |# 42") => 42)

;; Datum comment (#; <intertoken space> <datum>)
(check (read-str "#;ignored 42") => 42)
(check (read-str "(1 #;(2 3) 4)") => '(1 4))
(check (read-str "#;#|comment|#bad 42") => 42)
(check (read-str "(a #;(b (c d)) e)") => '(a e))
(check (eof-object? (read-str "#;42")) => #t)

;; Comments may appear between tokens
(check (read-all "1 #| c |#2") => '(1 2))
(check (read-all "1; comment\n2") => '(1 2))
(check (read-all "1 #; 2 3") => '(1 3))

;; String markers inside a block comment are not nested comments
(check (read-str "#| \"#| not nested |#\" |# 42") => 42)

;; =============================================================================
;; 2.4  Datum Labels (shared / circular structure)
;; =============================================================================

(check (read-str "(#0=10 #0#)") => '(10 10))

;; Shared structure (same object)
(let ((d (read-str "(#0=(1 2) #0#)")))
  (check (eq? (car d) (cadr d)) => #t))
(let ((v (read-str "(#0=#(1) #0#)")))
  (check (eq? (car v) (cadr v)) => #t))

;; Circular structure
(let ((cyclic (read-str "#0=(1 . #0#)")))
  (check (car cyclic) => 1)
  (check (eq? cyclic (cdr cyclic)) => #t))

;; =============================================================================
;; 2.1  Reader directives: #!fold-case / #!no-fold-case
;; =============================================================================

(check (read-all "#!fold-case Foo") => '(foo))
(check (read-all "#!fold-case Foo bar") => '(foo bar))
(check (read-all "#!fold-case Foo #!no-fold-case Bar") => '(foo Bar))
(check (read-all "#!fold-case #\\A") => '(#\A))

;; =============================================================================
;; 2.2  Whitespace
;; =============================================================================

(check (read-str "  42  ") => 42)
(check (eof-object? (read-str "")) => #t)
(check (eof-object? (read-str "   \n\t  ")) => #t)
(check (read-all "1 2 3") => '(1 2 3))
(check (read-all "(1 2) 3") => '((1 2) 3))

;; =============================================================================
;; Extension beyond R7RS-small: square brackets [ ] (supported by this reader,
;; equivalent to parentheses)
;; =============================================================================

(check (read-str "[1 2 3]") => '(1 2 3))
(check (read-str "[[1] 2]") => '((1) 2))

;; =============================================================================
;; Error cases (all raise 'read-error)
;; =============================================================================

;; --- Structural errors ---
(check-catch 'read-error (read-str "(1 2"))
(check-catch 'read-error (read-str "("))
(check-catch 'read-error (read-str ")"))
(check-catch 'read-error (read-str "\"abc"))
(check-catch 'read-error (read-str "\"abc\\"))
(check-catch 'read-error (read-str "{"))
(check-catch 'read-error (read-str "{1 2}"))

;; --- Abbreviation prefix with no following datum ---
(check-catch 'read-error (read-str "'"))
(check-catch 'read-error (read-str "`"))
(check-catch 'read-error (read-str ","))
(check-catch 'read-error (read-str ",@"))

;; --- Dotted pair syntax ---
(check-catch 'read-error (read-str "(1 . 2 3)"))
(check-catch 'read-error (read-str "(1 2 .)"))
(check-catch 'read-error (read-str "."))

;; --- Sharp notation errors ---
(check-catch 'read-error (read-str "#"))
(check-catch 'read-error (read-str "#z"))
(check-catch 'read-error (read-str "#\\"))
(check-catch 'read-error (read-str "#\\unknown"))
(check-catch 'read-error (read-str "#\\Space"))
(check (read-str "#\\x") => #\x)
(check-catch 'read-error (read-str "#tfoo"))
(check-catch 'read-error (read-str "#\\afoo"))

;; --- Number errors ---
(check-catch 'read-error (read-str "#xZZ"))
(check-catch 'read-error (read-str "42abc"))
(check-catch 'read-error (read-str "1+"))
(check-catch 'read-error (read-str "3.14.15"))

;; --- String escape errors ---
(check-catch 'read-error (read-str "\"\\q\""))
(check-catch 'read-error (read-str "\"\\x41\""))

;; --- Bytevector errors ---
(check-catch 'read-error (read-str "#u8(256)"))
(check-catch 'read-error (read-str "#u8(-1)"))
(check-catch 'read-error (read-str "#u8(1.5)"))
(check-catch 'read-error (read-str "#u8(1 2"))

;; --- Vector errors ---
(check-catch 'read-error (read-str "#(1 . 2)"))

;; --- Datum comment errors ---
(check-catch 'read-error (read-str "#;"))
(check-catch 'read-error (read-str "#; "))

;; --- Datum label errors ---
(check-catch 'read-error (read-str "#1#"))
(check-catch 'read-error (read-str "#0=#0#"))
(check-catch 'read-error (read-str "(#0# #0=a)"))

;; --- Directive errors ---
(check-catch 'read-error (read-str "#!fold-caseX"))

;; --- Other invalid tokens ---
(check-catch 'read-error (read-str "a#b"))

;; =============================================================================
;; 6.2.5  Radix/exactness prefixes with complex forms
;; =============================================================================

;; rational with radix prefix
(check (read-str "#x1/2") => 1/2)
;; pure imaginary with exactness prefix (real part omitted)
(check (= (read-str "#e+2i") (make-rectangular 0 2)) => #t)
(check (= (read-str "#i-1.5i") (make-rectangular 0 -1.5)) => #t)
(check (= (read-str "#e-i") (make-rectangular 0 -1)) => #t)
;; #e/#i on a complex: S7 cannot represent an exact complex, so the number is
;; returned unchanged rather than raising a foreign inexact->exact error
(check (= (read-str "#e1+2i") 1+2i) => #t)
(check (= (read-str "#i1+2i") 1+2i) => #t)
;; exactness still applies to reals
(check (exact? (read-str "#e42.0")) => #t)

;; =============================================================================
;; 6.3.1  Booleans are case-sensitive
;; =============================================================================

(check-catch 'read-error (read-str "#T"))
(check-catch 'read-error (read-str "#F"))
(check-catch 'read-error (read-str "#TRUE"))
(check-catch 'read-error (read-str "#FALSE"))

;; =============================================================================
;; 6.7  CRLF line continuation inside strings
;; =============================================================================

(check (read-str "\"a\\\r\n b\"") => "ab")
(check (read-str "\"a\\\r b\"") => "ab")
(check (read-str "\"a\\\n b\"") => "ab")

;; =============================================================================
;; 7.1.1  | is not a delimiter
;; =============================================================================

(check-catch 'read-error (read-str "foo|bar|"))
(check-catch 'read-error (read-str "a|b"))

;; =============================================================================
;; Internal objects (#<eof> / #<unspecified> round-trip)
;; =============================================================================

(check (eq? (read-str "#<eof>") (eof-object)) => #t)
(check (unspecified? (read-str "#<unspecified>")) => #t)
(check (undefined? (read-str "#<undefined>")) => #t)
;; any other #<name> is a named undefined (case* patterns etc.); the name
;; keeps its ">" and equal? compares by name
(check (undefined? (read-str "#<x:>")) => #t)
(check (string=? (object->string (read-str "#<x:>")) "#<x:>") => #t)
(check (string=? (object->string (read-str "#<integer?>")) "#<integer?>") => #t)
(check (string=? (object->string (read-str "#<...>")) "#<...>") => #t)
(check (equal? (read-str "#<x:>") (read-str "#<x:>")) => #t)

;; =============================================================================
;; SRFI-267 raw strings: #"delimiter"body"delimiter"
;; =============================================================================

(check (read-str "#\"\"\"\"") => "")
(check (read-str "#\"\" \"\"") => " ")
(check (read-str "#\"\"a\"\"") => "a")
(check (read-str "#\"\"\\\"\"") => "\\")
(check (read-str "#\"-\"\"\"-\"") => "\"")
(check (read-str "#\"-\"\\\"\"-\"") => "\\\"")
(check (read-str "#\"(())\"value\"(())\"") => "value")
(check (read-str "#\"tag with space\"hello\"tag with space\"") => "hello")
(check (read-str "#\"\"a\"b\"\"") => "a\"b")
(check-catch 'read-error (read-str "#\"\"a"))

;; =============================================================================
;; Nesting depth guard: deep nesting must read fine up to the limit and raise
;; a catchable read-error beyond it (previously this segfaulted).
;; =============================================================================

(define (deep-nested n)
  (string-append (make-string n #\() (make-string n #\))))

(check (pair? (read-str (deep-nested 100))) => #t)
(check (pair? (read-str (deep-nested 20000))) => #t)
(check (pair? (read-str (deep-nested 39000))) => #t)
(check-catch 'read-error (read-str (deep-nested 41000)))

;; the limit also applies to vector and quote abbreviation nesting
(define (deep-vectors n)
  (let loop ((i 0) (acc ""))
    (if (= i n)
      (string-append acc (make-string n #\)))
      (loop (+ i 1) (string-append acc "#(")))))
(check (vector? (read-str (deep-vectors 100))) => #t)
(check-catch 'read-error (read-str (deep-vectors 41000)))

;; =============================================================================
;; Round-trip: object->string output must be readable and read back equal
;; =============================================================================

(define (round-trip datum)
  ;; write datum, read it back, check equality.  Unreadable output (e.g. a
  ;; procedure printed as #<...>) is reported as 'skip.
  (let ((s (object->string datum)))
    (catch #t
      (lambda ()
        (let ((d2 (read (open-input-string s))))
          (if (or (equal? datum d2)
                  ;; NaN is never eqv? to itself
                  (and (number? datum) (number? d2) (nan? datum) (nan? d2)))
            'ok
            'mismatch)))
      (lambda args 'skip))))

(define round-trip-values
  (list 0 42 -100 3.14 -0.5 1/3 22/7 3+4i 1-1i +i -2i 1@2
    #b101010 #o52 #x2A #e42.0 #i1/2 +inf.0 -inf.0 +nan.0
    #\a #\newline #\space #\alarm #\x3bb #\(
    "hello" "a\nb" "quote: \"inside\"" "back\\slash" "tab\t" "end;"
    "#| not comment |#" "#; not comment" "#(not a vector)"
    'foo '->string '... '+soup+ '! '@
    '(a b c) '(1 . 2) '(1 2 . 3) '((a) (b (c)))
    #(1 2 "three" 'sym) #(#(1) #(2))
    #u8(0 127 255)
    '(quote x) '(quasiquote (a (unquote b)))
    #t #f))

(define round-trip-failures '())
(for-each
  (lambda (v)
    (let ((r (round-trip v)))
      (when (not (eq? r 'ok))
        (set! round-trip-failures (cons (list v r (object->string v)) round-trip-failures)))))
  round-trip-values)

(check (null? round-trip-failures) => #t)
(for-each (lambda (f) (format () "round-trip failure: ~S\n" f)) round-trip-failures)

;; cyclic structure round-trips through datum labels
(let ((cyc (read-str "#0=(1 . #0#)")))
  (check (equal? cyc (read (open-input-string (object->string cyc)))) => #t))
;; shared structure: S7's write does not emit labels for non-cyclic sharing,
;; so a round-trip preserves values but not identity
(let* ((shared (read-str "(#0=(1 2) #0#)"))
       (again (read (open-input-string (object->string shared)))))
  (check (equal? again shared) => #t))

;; =============================================================================
;; Fuzz: any random input must either read or raise a catchable error,
;; never crash or hang
;; =============================================================================

(define fuzz-chunks
  (list "#(" "#u8(" "#x" "#\\" "#;" "#!" "'" "`" ",@" "(" ")" "[" "]"
    "\"" "\\" ";" " " "1" "2" "3" "a" "b" "z" "+" "-" "." "|"
    "e" "i" "0" "9" ":" "@" "!" "?" "5" "7" "8" "6" "4" "#"))

(define (fuzz-input)
  (let loop ((n (+ 1 (random 200))) (acc '()))
    (if (= n 0)
      (apply string-append (reverse acc))
      (loop (- n 1)
            (cons (list-ref fuzz-chunks (random (length fuzz-chunks))) acc)))))

(define fuzz-read 0)
(define fuzz-error 0)
(do ((i 0 (+ i 1))) ((= i 500))
  (let ((s (fuzz-input)))
    (catch #t
      (lambda () (read (open-input-string s)) (set! fuzz-read (+ fuzz-read 1)))
      (lambda args (set! fuzz-error (+ fuzz-error 1))))))

(check (+ fuzz-read fuzz-error) => 500)
(check (>= fuzz-read 0) => #t)

;; read errors carry the port byte position
(let ((args (catch 'read-error
              (lambda () (read (open-input-string "(1 2")))
              (lambda (tag . errs) (car errs)))))
  (check (pair? args) => #t)
  (check (string? (car args)) => #t)
  (check (> (string-length (car args)) (string-length "unexpected end of input")) => #t)
  (check (string>? (car args) "unexpected end of input") => #t))

(check-report)
