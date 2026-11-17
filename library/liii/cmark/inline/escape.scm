
(define-library (liii cmark inline escape)
  (import (scheme base))
  (import (liii cmark node))
  (import (liii cmark text))
  (export backslash-char? backslash-hard-linebreak? hard-linebreak-by-spaces?
    parse-backslash-escape decode-backslash-escapes-in-string
  ) ;export
  (begin
    (define (backslash-char? str pos)
      (and (< pos (string-length str)) (char=? (string-ref str pos) #\\))
    ) ;define

    (define (backslash-hard-linebreak? str pos)
      (and (< (+ pos 1) (string-length str))
        (char=? (string-ref str pos) #\\)
        (char=? (string-ref str (+ pos 1)) #\newline)
      ) ;and
    ) ;define

    (define (hard-linebreak-by-spaces? str pos)
      (and (< pos (string-length str))
        (char=? (string-ref str pos) #\newline)
        (>= pos 2)
        (char=? (string-ref str (- pos 1)) #\space)
        (char=? (string-ref str (- pos 2)) #\space)
      ) ;and
    ) ;define

    (define (ascii-punctuation? ch)
      (let ((n (char->integer ch)))
        (or (and (>= n 33) (<= n 47))
          (and (>= n 58) (<= n 64))
          (and (>= n 91) (<= n 96))
          (and (>= n 123) (<= n 126))
        ) ;or
      ) ;let
    ) ;define

    ;; CommonMark: 反斜杠只能转义 ASCII 标点，不能转义空格（Example 13 的 \ ）
    (define (backslash-escapable-char? ch)
      (ascii-punctuation? ch)
    ) ;define

    (define (parse-backslash-escape str start)
      (let ((len (string-length str)))
        (cond ((>= (+ start 1) len) (cons (make-cmark-text "\\") (+ start 1)))
              ((backslash-escapable-char? (string-ref str (+ start 1)))
               (cons
                 (make-cmark-text (string (string-ref str (+ start 1))))
                 (+ start 2)
               ) ;cons
              ) ;
              (else (cons (make-cmark-text "\\") (+ start 1)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 把字符串里的反斜杠转义规约为最终字符（用于 URL/title/info 等非行内上下文）
    (define (decode-backslash-escapes-in-string str)
      (let ((len (string-length str)))
        (let loop
          ((pos 0) (parts '()))
          (if (>= pos len)
            (string-join (reverse parts) "")
            (if (backslash-char? str pos)
              (let* ((result (parse-backslash-escape str pos))
                     (text (cmark-node-data-ref-escape (car result)))
                    ) ;
                (loop (cdr result) (cons text parts))
              ) ;let*
              (loop (+ pos 1) (cons (string (string-ref str pos)) parts))
            ) ;if
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (cmark-node-data-ref-escape node)
      (let ((cell (assoc 'content (cmark-node-data node))))
        (if cell (cdr cell) "")
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
