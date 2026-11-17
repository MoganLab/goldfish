
(define-library (liii utils markdown-alert-block-util)
  (import (scheme base))
  (import (scheme char))
  (import (liii utils text-util))
  (import (liii utils markdown-blockquote-util))
  (export alert-block-start? collect-alert-block-lines)
  (begin
    (define (string-upcase-ascii str)
      (list->string (map char-upcase (string->list str)))
    ) ;define

    (define (alert-kind-symbol label)
      (let ((upper (string-upcase-ascii label)))
        (cond ((string=? upper "NOTE") 'note)
              ((string=? upper "TIP") 'tip)
              ((string=? upper "IMPORTANT") 'important)
              ((string=? upper "WARNING") 'warning)
              ((string=? upper "CAUTION") 'caution)
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; alert 尾部：结束标记后是行尾或空白（§3.14 alert 语法）
    (define (alert-tail-ok? trimmed pos len)
      (or (= pos len) (char-whitespace? (string-ref trimmed pos)))
    ) ;define

    (define (parse-alert-marker-line line)
      (let* ((trimmed (string-trim-left line)) (len (string-length trimmed)))
        (if (and (>= len 4)
              (char=? (string-ref trimmed 0) #\[)
              (char=? (string-ref trimmed 1) #\!)
            ) ;and
          (let loop
            ((i 2))
            (cond ((>= i len) #f)
                  ((char=? (string-ref trimmed i) #\])
                   (let* ((kind (alert-kind-symbol (substring trimmed 2 i)))
                          (tail-start (+ i 1))
                          (tail-rest (substring trimmed tail-start len))
                          (tail-text (if (= tail-start len) "" (string-trim-left tail-rest)))
                          (ok? (and kind (alert-tail-ok? trimmed tail-start len)))
                         ) ;
                     (if ok? (cons kind tail-text) #f)
                   ) ;let*
                  ) ;
                  ((char-whitespace? (string-ref trimmed i)) #f)
                  (else (loop (+ i 1)))
            ) ;cond
          ) ;let
          #f
        ) ;if
      ) ;let*
    ) ;define

    (define (alert-block-start? line)
      (and (blockquote-line? line)
        (parse-alert-marker-line (unquote-blockquote-line line))
        #t
      ) ;and
    ) ;define

    (define (collect-alert-block-lines lines)
      (let* ((parsed (collect-blockquote-lines lines))
             (quoted-lines (car parsed))
             (remaining (cdr parsed))
             (unquoted-lines (map unquote-blockquote-line quoted-lines))
             (marker (parse-alert-marker-line (car unquoted-lines)))
             (kind (car marker))
             (head-tail (cdr marker))
             (content-lines (if (string=? head-tail "")
                              (cdr unquoted-lines)
                              (cons head-tail (cdr unquoted-lines))
                            ) ;if
             ) ;content-lines
            ) ;
        (cons `((kind . ,kind) (lines . ,content-lines)) remaining)
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library
