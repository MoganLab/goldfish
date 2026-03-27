(define-library (liii goldfix)
  (import (scheme base))
  (import (liii sys))
  (export main)
  (export count-content-paren-issues)

  (begin
    (define (char-literal-delimiter? ch)
      (or (char-whitespace? ch)
          (char=? ch #\()
          (char=? ch #\))
          (char=? ch #\")
          (char=? ch #\;)
      ) ;or
    ) ;define

    (define (skip-char-literal-index content start)
      (let ((len (string-length content)))
        (if (>= (+ start 2) len)
          len
          (let loop ((i (+ start 3)))
            (if (or (>= i len)
                    (char-literal-delimiter? (string-ref content i)))
              i
              (loop (+ i 1))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (read-file-content file-path)
      (call-with-input-file file-path
        (lambda (port)
          (let loop ((lines '()) (line (read-line port)))
            (if (eof-object? line)
              (let ((lst (reverse lines)))
                (if (null? lst)
                  ""
                  (let join-loop ((rest (cdr lst)) (result (car lst)))
                    (if (null? rest)
                      result
                      (join-loop (cdr rest) (string-append result "\n" (car rest)))
                    ) ;if
                  ) ;let
                ) ;if
              ) ;let
              (loop (cons line lines) (read-line port))
            ) ;if
          ) ;let
        ) ;lambda
      ) ;call-with-input-file
    ) ;define

    (define (count-content-paren-issues content)
      (let ((len (string-length content)))
        (let loop ((i 0)
                   (balance 0)
                   (extra-right-parens 0)
                   (block-depth 0)
                   (in-string #f)
                   (escape-next #f)
                   (in-line-comment #f))
          (if (>= i len)
            (cons extra-right-parens balance)
            (let ((ch (string-ref content i)))
              (cond
                (in-line-comment
                 (if (char=? ch #\newline)
                   (loop (+ i 1)
                         balance
                         extra-right-parens
                         block-depth
                         in-string
                         #f
                         #f
                   ) ;loop
                   (loop (+ i 1)
                         balance
                         extra-right-parens
                         block-depth
                         in-string
                         #f
                         #t
                   ) ;loop
                 ) ;if
                ) ;in-line-comment

                (escape-next
                 (loop (+ i 1)
                       balance
                       extra-right-parens
                       block-depth
                       in-string
                       #f
                       #f
                 ) ;loop
                ) ;escape-next

                ((> block-depth 0)
                 (cond
                   ((and (< (+ i 1) len)
                         (char=? ch #\#)
                         (char=? (string-ref content (+ i 1)) #\|))
                    (loop (+ i 2)
                          balance
                          extra-right-parens
                          (+ block-depth 1)
                          in-string
                          #f
                          #f
                    ) ;loop
                   ) ;
                   ((and (< (+ i 1) len)
                         (char=? ch #\|)
                         (char=? (string-ref content (+ i 1)) #\#))
                    (loop (+ i 2)
                          balance
                          extra-right-parens
                          (- block-depth 1)
                          in-string
                          #f
                          #f
                    ) ;loop
                   ) ;
                   (else
                    (loop (+ i 1)
                          balance
                          extra-right-parens
                          block-depth
                          in-string
                          #f
                          #f
                    ) ;loop
                   ) ;else
                 ) ;cond
                ) ;block-depth

                (in-string
                 (cond
                   ((char=? ch #\\)
                    (loop (+ i 1)
                          balance
                          extra-right-parens
                          block-depth
                          #t
                          #t
                          #f
                    ) ;loop
                   ) ;
                   ((char=? ch #\")
                    (loop (+ i 1)
                          balance
                          extra-right-parens
                          block-depth
                          #f
                          #f
                          #f
                    ) ;loop
                   ) ;
                   (else
                    (loop (+ i 1)
                          balance
                          extra-right-parens
                          block-depth
                          #t
                          #f
                          #f
                    ) ;loop
                   ) ;else
                 ) ;cond
                ) ;in-string

                (else
                 (cond
                   ((and (< (+ i 1) len)
                         (char=? ch #\#)
                         (char=? (string-ref content (+ i 1)) #\|))
                    (loop (+ i 2)
                          balance
                          extra-right-parens
                          (+ block-depth 1)
                          #f
                          #f
                          #f
                    ) ;loop
                   ) ;
                   ((and (< (+ i 1) len)
                         (char=? ch #\#)
                         (char=? (string-ref content (+ i 1)) #\\))
                    (loop (skip-char-literal-index content i)
                          balance
                          extra-right-parens
                          block-depth
                          #f
                          #f
                          #f
                    ) ;loop
                   ) ;
                   ((char=? ch #\")
                    (loop (+ i 1)
                          balance
                          extra-right-parens
                          block-depth
                          #t
                          #f
                          #f
                    ) ;loop
                   ) ;
                   ((char=? ch #\;)
                    (loop (+ i 1)
                          balance
                          extra-right-parens
                          block-depth
                          #f
                          #f
                          #t
                    ) ;loop
                   ) ;
                   ((char=? ch #\()
                    (loop (+ i 1)
                          (+ balance 1)
                          extra-right-parens
                          block-depth
                          #f
                          #f
                          #f
                    ) ;loop
                   ) ;
                   ((char=? ch #\))
                    (if (> balance 0)
                      (loop (+ i 1)
                            (- balance 1)
                            extra-right-parens
                            block-depth
                            #f
                            #f
                            #f
                      ) ;loop
                      (loop (+ i 1)
                            0
                            (+ extra-right-parens 1)
                            block-depth
                            #f
                            #f
                            #f
                      ) ;loop
                    ) ;if
                   ) ;
                   (else
                    (loop (+ i 1)
                          balance
                          extra-right-parens
                          block-depth
                          #f
                          #f
                          #f
                    ) ;loop
                   ) ;else
                 ) ;cond
                ) ;else
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (display-file-paren-issues file-path)
      (let* ((counts (count-content-paren-issues (read-file-content file-path)))
             (extra-right-parens (car counts))
             (missing-right-parens (cdr counts)))
        (display file-path)
        (display ": extra-right-parens=")
        (display extra-right-parens)
        (display ", missing-right-parens=")
        (display missing-right-parens)
        (newline)
      ) ;let*
    ) ;define

    (define (main)
      (let ((args (cddr (argv))))
        (cond
          ((null? args)
           (display "Error: missing file path")
           (newline)
           (display "Usage: goldfix <PATH>")
           (newline)
          ) ;

          ((not (null? (cdr args)))
           (display "Error: only one file path is supported")
           (newline)
          ) ;

          (else
           (display-file-paren-issues (car args))
          ) ;else
        ) ;cond
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
