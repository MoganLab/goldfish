
(define-library (liii cmark stem-reader)
  (import (scheme base))
  (import (scheme char))
  (import (scheme read))
  (import (liii cmark text))
  (export read-md-stem)
  (begin
    (define (read-md-stem . maybe-port)
      (let ((port
              (cond ((null? maybe-port) (current-input-port))
                    ((null? (cdr maybe-port)) (car maybe-port))
                    (else (error 'wrong-number-of-args))
              ) ;cond
            ) ;port
           ) ;
        (cons 'md-stem
          (read-md-stem-forms (open-input-string (expand-md-stem-code-blocks (port->string port)))
          ) ;read-md-stem-forms
        ) ;cons
      ) ;let
    ) ;define

    (define (read-md-stem-forms port)
      (let loop
        ((forms '()))
        (let ((form (read port)))
          (if (eof-object? form) (reverse forms) (loop (cons form forms)))
        ) ;let
      ) ;let
    ) ;define

    (define (expand-md-stem-code-blocks source)
      (let loop
        ((remaining (string-split-lines source)) (output-lines '()))
        (cond ((null? remaining) (string-join (reverse output-lines) "\n"))
              ((code-block-start-line? (car remaining))
               (let* ((parsed (rewrite-md-stem-code-block remaining))
                      (line (car parsed))
                      (rest-lines (cdr parsed))
                     ) ;
                 (loop rest-lines (cons line output-lines))
               ) ;let*
              ) ;
              (else (loop (cdr remaining) (cons (car remaining) output-lines)))
        ) ;cond
      ) ;let
    ) ;define

    (define (code-block-start-line? line)
      (let* ((trimmed (string-trim-right (string-trim-left line)))
             (prefix "(code-block #\"")
             (prefix-len (string-length prefix))
             (len (string-length trimmed))
            ) ;
        (and (>= len (+ prefix-len 1))
          (string-prefix? prefix trimmed)
          (char=? (string-ref trimmed (- len 1)) #\")
        ) ;and
      ) ;let*
    ) ;define
    (define (code-block-line-prefix line)
      (let loop
        ((chars (string->list line)) (prefix '()))
        (cond ((null? chars) (list->string (reverse prefix)))
              ((char-whitespace? (car chars)) (loop (cdr chars) (cons (car chars) prefix)))
              (else (list->string (reverse prefix)))
        ) ;cond
      ) ;let
    ) ;define
    (define (code-block-start-delimiter line)
      (let* ((trimmed (string-trim-right (string-trim-left line)))
             (prefix-len (string-length "(code-block #\""))
             (len (string-length trimmed))
            ) ;
        (substring trimmed prefix-len (- len 1))
      ) ;let*
    ) ;define

    (define (code-block-end-line? delimiter line)
      (string=? (string-trim-right (string-trim-left line))
        (string-append "\"" delimiter "\") ;code-block")
      ) ;string=?
    ) ;define

    (define (rewrite-md-stem-code-block lines)
      (let ((delimiter (code-block-start-delimiter (car lines)))
            (prefix (code-block-line-prefix (car lines)))
           ) ;
        (let loop
          ((remaining (cdr lines)) (content-lines '()))
          (cond ((null? remaining)
                 (error 'read-md-stem "unterminated code-block in md.stem resource")
                ) ;
                ((code-block-end-line? delimiter (car remaining))
                 (cons
                   (string-append prefix
                     "(code-block "
                     (scheme-string-literal delimiter)
                     " "
                     (scheme-string-literal (string-join (reverse content-lines) "\n"))
                     ")"
                   ) ;string-append
                   (cdr remaining)
                 ) ;cons
                ) ;
                (else (loop (cdr remaining) (cons (car remaining) content-lines)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (scheme-string-literal str)
      (string-append "\"" (escape-scheme-string str) "\"")
    ) ;define

    (define (escape-scheme-string str)
      (let loop
        ((chars (string->list str)) (parts '()))
        (if (null? chars)
          (string-join (reverse parts) "")
          (let ((part
                  (cond ((char=? (car chars) #\\) "\\\\")
                        ((char=? (car chars) #\") "\\\"")
                        ((char=? (car chars) #\newline) "\\n")
                        (else (string (car chars)))
                  ) ;cond
                ) ;part
               ) ;
            (loop (cdr chars) (cons part parts))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (port->string port)
      (let loop
        ((chars '()))
        (let ((ch (read-char port)))
          (if (eof-object? ch) (list->string (reverse chars)) (loop (cons ch chars)))
        ) ;let
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
