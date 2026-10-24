(import (liii check))
(import (liii goldfmt format datum) (liii goldfmt format reader) (liii goldfmt record)
  (srfi srfi-13))

(check-set-mode! 'report-failed)

;; single-arg-symbol-form?
;; 判断 value 是否为 (name x) 形式的单参数表单。
(check (single-arg-symbol-form? '(quasiquote x) 'quasiquote) => #t)
(check (single-arg-symbol-form? '(unquote a b) 'unquote) => #f)
(check (single-arg-symbol-form? '(foo x) 'bar) => #f)
(check (single-arg-symbol-form? 'sym 'sym) => #f)

;; quote-syntax-form?
;; 判断 value 是否为 #_quote 语法对象包裹的单参数表单。
(let ((q (read (open-input-string "'x"))))
  (check (quote-syntax-form? q) => #t)
) ;let
(check (quote-syntax-form? '(a b)) => #f)

;; raw-string-datum? / newline-marker-datum? / comment-datum?
(check (raw-string-datum? '(*raw-string* "#\"x\"" "x")) => #t)
(check (raw-string-datum? '(*raw-string* "#\"x\"")) => #f)
(check (newline-marker-datum? '(*newline* 2)) => #t)
(check (newline-marker-datum? '(*newline* x)) => #f)
(check (comment-datum? '(*comment* "note")) => #t)
(check (comment-datum? '(*comment*)) => #f)

;; reader-newlines
(check (reader-newlines 0) => "")
(check (reader-newlines 2) => "\n\n")

;; reader-datum-contains-newline-marker?
(check (reader-datum-contains-newline-marker? '(*newline* 1)) => #t)
(check (reader-datum-contains-newline-marker? '(a (*newline* 2))) => #t)
(check (reader-datum-contains-newline-marker? '(a b)) => #f)
(check (reader-datum-contains-newline-marker? #((*newline* 1))) => #t)
(check (reader-datum-contains-newline-marker? #(a b)) => #f)

;; reader-head-name
(check (reader-head-name 'car) => "car")
(check (reader-head-name 42) => "default")

;; reader-rule-head? / reader-form-like?
(check (reader-rule-head? 'car) => #t)
(check (reader-rule-head? 42) => #f)
(check (reader-form-like? '(a)) => #t)
(check (reader-form-like? 'a) => #f)

;; reader-selected-item / reader-selected-column / reader-selected-text
(let ((sel (list 'item 5 "text")))
  (check (reader-selected-item sel) => 'item)
  (check (reader-selected-column sel) => 5)
  (check (reader-selected-text sel) => "text")
) ;let

;; reader-skip-selected
(check (reader-skip-selected '(a b c) '((a) (b))) => '(c))
(check (reader-skip-selected '(a) '()) => '(a))

;; reader-first-selected-form-column
(check (reader-first-selected-form-column '()) => #f)
(check (reader-first-selected-form-column (list (list 'atom 3 "x"))) => #f)
(check (reader-first-selected-form-column (list (list '(f) 4 "x"))) => 4)

;; reader-by-first-rest-child-indent
(check (reader-by-first-rest-child-indent 0 '((f) x)) => 2)
(check (reader-by-first-rest-child-indent 0 '(atom x)) => 2)

;; reader-rest-indent
(check (reader-rest-indent "lambda" 0 '() '()) => 2)

;; reader-append-selected
(check (reader-append-selected "(" (list (list 'a 1 "a") (list 'b 2 "b"))) => "( a b")
(check (reader-append-selected "(" '()) => "(")

;; last-line-column
(check (last-line-column "abc") => 3)
(check (last-line-column "ab\ncd") => 2)

;; reader-fill-atom? / reader-fill-eligible?
(check (reader-fill-atom? 'a) => #t)
(check (reader-fill-atom? '(a)) => #f)
(check (reader-fill-atom? #(a)) => #f)
(check (reader-fill-eligible? '(a b c d)) => #t)
(check (reader-fill-eligible? '(a b c)) => #f)
(check (reader-fill-eligible? '(a b #:k c d)) => #f)

;; reader-append-close
(check (reader-append-close "(a" 0) => "(a)")
(check (reader-append-close "(a ;#" 2) => "(a ;#\n  )")

;; reader-vector-prefix
(check (reader-vector-prefix #(1)) => "#(")
(check (reader-vector-prefix (make-byte-vector 2 0)) => "#u8(")

;; format-reader-pair-fill（fmt 过程参数化）
(check (format-reader-pair-fill '(a b c d e) 0 format-reader-datum-inline) => "(a b c d e)")

;; reader-keyword-block-info（fmt 过程参数化）
(call-with-values
  (lambda ()
    (reader-keyword-block-info (list #:k1 'v1 #:k2 'v2) 0 format-reader-datum-inline))
  (lambda (count max-key-len)
    (check count => 2)
    (check max-key-len => 3)
  ) ;lambda
) ;call-with-values

(check-report)
