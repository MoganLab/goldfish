;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core session-stats token-stats)
  (import (scheme base) (liii base))
  (export make-pi-session-token-stats pi-session-token-stats?
    pi-session-token-stats-input pi-session-token-stats-output
    pi-session-token-stats-cache-read pi-session-token-stats-cache-write
    pi-session-token-stats-total pi-session-token-stats-add-usage!
  ) ;export
  (begin

    (define*-record-type <pi-session-token-stats>
      (make-pi-session-token-stats (input 0) (output 0) (cache-read 0) (cache-write 0))
      pi-session-token-stats?
      (input pi-session-token-stats-input pi-session-token-stats-input-set!)
      (output pi-session-token-stats-output pi-session-token-stats-output-set!)
      (cache-read pi-session-token-stats-cache-read
        pi-session-token-stats-cache-read-set!
      ) ;cache-read
      (cache-write pi-session-token-stats-cache-write
        pi-session-token-stats-cache-write-set!
      ) ;cache-write
    ) ;define*-record-type

    (define (%ref obj key default)
      (if (not (pair? obj))
        default
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (or (assoc key obj) (assoc str-key obj) (assoc sym-key obj)))
              ) ;
          (if (pair? pair) (cdr pair) default)
        ) ;let*
      ) ;if
    ) ;define

    (define (pi-session-token-stats-total tokens)
      (+ (pi-session-token-stats-input tokens)
        (pi-session-token-stats-output tokens)
        (pi-session-token-stats-cache-read tokens)
        (pi-session-token-stats-cache-write tokens)
      ) ;+
    ) ;define

    (define (pi-session-token-stats-add-usage! tokens usage)
      (pi-session-token-stats-input-set! tokens
        (+ (pi-session-token-stats-input tokens) (%ref usage 'input 0))
      ) ;pi-session-token-stats-input-set!
      (pi-session-token-stats-output-set! tokens
        (+ (pi-session-token-stats-output tokens) (%ref usage 'output 0))
      ) ;pi-session-token-stats-output-set!
      (pi-session-token-stats-cache-read-set! tokens
        (+ (pi-session-token-stats-cache-read tokens) (%ref usage 'cacheRead 0))
      ) ;pi-session-token-stats-cache-read-set!
      (pi-session-token-stats-cache-write-set! tokens
        (+ (pi-session-token-stats-cache-write tokens) (%ref usage 'cacheWrite 0))
      ) ;pi-session-token-stats-cache-write-set!
    ) ;define

  ) ;begin
) ;define-library
