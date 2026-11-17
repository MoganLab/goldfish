
(define-library (liii cmark inline emoji)
  (import (scheme base))
  (import (scheme char))
  (import (liii cmark node))
  (export emoji-candidate-start? emoji-start? parse-emoji)
  (begin
    (define supported-emoji-shortcodes
      '("smile" "grin" "joy" "heart" "fire" "rocket" "warning" "tada" "thinking"
        "eyes" "sparkles" "white_check_mark" "x" "+1" "thumbsup")
    ) ;define

    (define (emoji-shortcode-char? ch)
      (or (char-alphabetic? ch)
        (char-numeric? ch)
        (char=? ch #\_)
        (char=? ch #\+)
        (char=? ch #\-)
      ) ;or
    ) ;define

    (define (emoji-candidate-start? str pos)
      (and (< pos (string-length str))
        (char=? (string-ref str pos) #\:)
        (< (+ pos 1) (string-length str))
        (emoji-shortcode-char? (string-ref str (+ pos 1)))
      ) ;and
    ) ;define

    (define (supported-emoji-shortcode? shortcode)
      (if (member shortcode supported-emoji-shortcodes) #t #f)
    ) ;define

    (define (find-emoji-end str start)
      (let ((len (string-length str)))
        (let loop
          ((pos (+ start 1)))
          (cond ((>= pos len) #f)
                ((char=? (string-ref str pos) #\:)
                 (let ((shortcode (substring str (+ start 1) pos)))
                   (and (> (string-length shortcode) 0) (supported-emoji-shortcode? shortcode) pos)
                 ) ;let
                ) ;
                ((emoji-shortcode-char? (string-ref str pos)) (loop (+ pos 1)))
                (else #f)
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (emoji-start? str pos)
      (and (emoji-candidate-start? str pos) (find-emoji-end str pos) #t)
    ) ;define

    (define (parse-emoji str start)
      (let* ((len (string-length str)) (end-pos (find-emoji-end str start)))
        (if end-pos
          (let ((shortcode (substring str (+ start 1) end-pos)))
            (cons (make-cmark-emoji shortcode) (+ end-pos 1))
          ) ;let
          (cons (make-cmark-text (substring str start len)) len)
        ) ;if
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library
