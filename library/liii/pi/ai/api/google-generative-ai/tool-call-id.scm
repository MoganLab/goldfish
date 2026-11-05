;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api google-generative-ai tool-call-id)
  (import (scheme base) (scheme char))
  (export gemini-normalize-tool-call-id gemini-generate-tool-call-id)
  (begin

    (define (%map-char c)
      (if (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_) (char=? c #\-))
        c
        #\_
      ) ;if
    ) ;define

    (define (gemini-normalize-tool-call-id id)
      (if (not (string? id))
        "tool_call_0"
        (let ((res (list->string (map %map-char (string->list id)))))
          (if (> (string-length res) 64) (substring res 0 64) res)
        ) ;let
      ) ;if
    ) ;define

    (define* (gemini-generate-tool-call-id (name "call") (timestamp 0) (counter 1))
      (gemini-normalize-tool-call-id (string-append name "_" (number->string timestamp) "_" (number->string counter))
      ) ;gemini-normalize-tool-call-id
    ) ;define*

  ) ;begin
) ;define-library
