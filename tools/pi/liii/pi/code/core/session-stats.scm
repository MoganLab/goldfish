;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; /session 核心统计：从持久化 entries 聚合消息、token 与费用，并格式化为纯文本
(define-library (liii pi code core session-stats)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii list)
    (liii pi code core compaction session)
  ) ;import
  (export pi-get-session-stats pi-format-session-stats
    make-pi-session-token-stats pi-session-token-stats?
    pi-session-token-stats-input pi-session-token-stats-output
    pi-session-token-stats-cache-read pi-session-token-stats-cache-write
    pi-session-token-stats-total
  ) ;export
  (begin

    (define-record-type <pi-session-token-stats>
      (make-pi-session-token-stats input output cache-read cache-write)
      pi-session-token-stats?
      (input pi-session-token-stats-input pi-session-token-stats-input-set!)
      (output pi-session-token-stats-output pi-session-token-stats-output-set!)
      (cache-read pi-session-token-stats-cache-read
        pi-session-token-stats-cache-read-set!
      ) ;cache-read
      (cache-write pi-session-token-stats-cache-write
        pi-session-token-stats-cache-write-set!
      ) ;cache-write
    ) ;define-record-type

    (define (pi-session-token-stats-total tokens)
      (+ (pi-session-token-stats-input tokens)
        (pi-session-token-stats-output tokens)
        (pi-session-token-stats-cache-read tokens)
        (pi-session-token-stats-cache-write tokens)
      ) ;+
    ) ;define

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

    (define (%content-list content)
      (cond ((vector? content) (vector->list content))
            ((list? content) content)
            (else '())
      ) ;cond
    ) ;define

    (define (%add-usage! totals usage)
      (vector-set! totals 5 (+ (vector-ref totals 5) (%ref usage 'input 0)))
      (vector-set! totals 6 (+ (vector-ref totals 6) (%ref usage 'output 0)))
      (vector-set! totals 7 (+ (vector-ref totals 7) (%ref usage 'cacheRead 0)))
      (vector-set! totals 8 (+ (vector-ref totals 8) (%ref usage 'cacheWrite 0)))
      (vector-set! totals
        9
        (+ (vector-ref totals 9) (%ref (%ref usage 'cost '()) 'total 0.0))
      ) ;vector-set!
    ) ;define

    (define (%collect-message! totals msg)
      (let ((role (%ref msg 'role "")))
        (cond
         ((equal? role "user") (vector-set! totals 1 (+ (vector-ref totals 1) 1)))
         ((equal? role "assistant")
          (vector-set! totals 2 (+ (vector-ref totals 2) 1))
          (vector-set! totals
            3
            (+ (vector-ref totals 3)
              (count (lambda (block) (equal? (%ref block 'type "") "toolCall"))
                (%content-list (%ref msg 'content '()))
              ) ;count
            ) ;+
          ) ;vector-set!
          (%add-usage! totals (%ref msg 'usage '()))
         ) ;
         ((equal? role "toolResult") (vector-set! totals 4 (+ (vector-ref totals 4) 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-get-session-stats file-path)
      (let* ((entries (if (and file-path (file-exists? file-path))
                        (pi-read-session-entries file-path)
                        '()
                      ) ;if
             ) ;entries
             ;; total, user, assistant, tool-calls, tool-results,
             ;; input, output, cache-read, cache-write, cost
             (totals (make-vector 10 0))
             (session-id #f)
             (session-name #f)
            ) ;
        (for-each
          (lambda (entry)
            (let ((type (%ref entry 'type "")))
              (cond ((equal? type "session") (set! session-id (%ref entry 'id #f)))
                    ((equal? type "session_info") (set! session-name (%ref entry 'name #f)))
                    ((equal? type "message")
                     (vector-set! totals 0 (+ (vector-ref totals 0) 1))
                     (%collect-message! totals (%ref entry 'message '()))
                    ) ;
              ) ;cond
            ) ;let
          ) ;lambda
          entries
        ) ;for-each
        (list (cons 'session-file file-path)
          (cons 'session-id session-id)
          (cons 'session-name session-name)
          (cons 'total-messages (vector-ref totals 0))
          (cons 'user-messages (vector-ref totals 1))
          (cons 'assistant-messages (vector-ref totals 2))
          (cons 'tool-calls (vector-ref totals 3))
          (cons 'tool-results (vector-ref totals 4))
          (cons 'tokens
            (make-pi-session-token-stats (vector-ref totals 5)
              (vector-ref totals 6)
              (vector-ref totals 7)
              (vector-ref totals 8)
            ) ;make-pi-session-token-stats
          ) ;cons
          (cons 'cost (vector-ref totals 9))
        ) ;list
      ) ;let*
    ) ;define

    (define (%comma-number n)
      (let* ((text (number->string n))
             (len (string-length text))
             (first-len
               (let ((r (remainder len 3)))
                 (if (zero? r) 3 r)
               ) ;let
             ) ;first-len
            ) ;
        (string-join
          (cons (substring text 0 first-len)
            (map (lambda (pos) (substring text pos (+ pos 3)))
              (iota (quotient (- len first-len) 3) first-len 3)
            ) ;map
          ) ;cons
          ","
        ) ;string-join
      ) ;let*
    ) ;define

    (define (%fixed n digits)
      (let* ((scale (expt 10 digits))
             (scaled (inexact->exact (round (* n scale))))
             (negative? (< scaled 0))
             (magnitude (abs scaled))
             (integer-part (quotient magnitude scale))
             (fraction-part (remainder magnitude scale))
             (fraction
               (if (zero? digits)
                 ""
                 (string-append "." (string-pad (number->string fraction-part) digits #\0))
               ) ;if
             ) ;fraction
            ) ;
        (string-append (if negative? "-" "") (number->string integer-part) fraction)
      ) ;let*
    ) ;define

    (define (pi-format-session-stats stats)
      (let* ((name (%ref stats 'session-name #f))
             (file (%ref stats 'session-file #f))
             (id (%ref stats 'session-id #f))
             (tokens (%ref stats 'tokens '()))
             (input (pi-session-token-stats-input tokens))
             (output (pi-session-token-stats-output tokens))
             (cache-read (pi-session-token-stats-cache-read tokens))
             (cache-write (pi-session-token-stats-cache-write tokens))
             (prompt-tokens (+ input cache-read cache-write))
             (cost (%ref stats 'cost 0.0))
             (lines
               (append (list "Session Info" "")
                 (if name (list (string-append "Name: " name)) '())
                 (list (string-append "File: " (or file "In-memory"))
                   (string-append "ID: " (or id "Unavailable"))
                   ""
                   "Messages"
                   (string-append "Total: " (number->string (%ref stats 'total-messages 0)))
                   (string-append "User: " (number->string (%ref stats 'user-messages 0)))
                   (string-append "Assistant: "
                     (number->string (%ref stats 'assistant-messages 0))
                   ) ;string-append
                   (string-append "Tools: "
                     (number->string (%ref stats 'tool-calls 0))
                     " calls, "
                     (number->string (%ref stats 'tool-results 0))
                     " results"
                   ) ;string-append
                   ""
                   "Tokens"
                   (string-append "Input: " (%comma-number prompt-tokens))
                 ) ;list
                 (if (and (> prompt-tokens 0) (> (+ cache-read cache-write) 0))
                   (list
                     (string-append "  Cached: "
                       (%comma-number cache-read)
                       " ("
                       (%fixed (* 100.0 (/ cache-read prompt-tokens)) 1)
                       "%)"
                     ) ;string-append
                     (string-append "  Uncached: "
                       (%comma-number (+ input cache-write))
                       (if (> cache-write 0)
                         (string-append " (" (%comma-number cache-write) " written to cache)")
                         ""
                       ) ;if
                     ) ;string-append
                   ) ;list
                   '()
                 ) ;if
                 (list (string-append "Output: " (%comma-number output))
                   (string-append "Total: " (%comma-number (pi-session-token-stats-total tokens)))
                 ) ;list
                 (if (> cost 0) (list "" "Cost" (string-append "Total: $" (%fixed cost 3))) '())
               ) ;append
             ) ;lines
            ) ;
        (string-join lines "\n")
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
