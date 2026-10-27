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
    pi-session-token-stats-total make-pi-session-stats pi-session-stats?
    pi-session-stats-session-file pi-session-stats-session-id
    pi-session-stats-session-name pi-session-stats-total-messages
    pi-session-stats-user-messages pi-session-stats-assistant-messages
    pi-session-stats-tool-calls pi-session-stats-tool-results
    pi-session-stats-tokens pi-session-stats-cost
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

    (define-record-type <pi-session-stats>
      (make-pi-session-stats session-file session-id session-name total-messages
        user-messages assistant-messages tool-calls tool-results tokens cost
      ) ;make-pi-session-stats
      pi-session-stats?
      (session-file pi-session-stats-session-file)
      (session-id pi-session-stats-session-id pi-session-stats-session-id-set!)
      (session-name pi-session-stats-session-name pi-session-stats-session-name-set!)
      (total-messages pi-session-stats-total-messages
        pi-session-stats-total-messages-set!
      ) ;total-messages
      (user-messages pi-session-stats-user-messages
        pi-session-stats-user-messages-set!
      ) ;user-messages
      (assistant-messages pi-session-stats-assistant-messages
        pi-session-stats-assistant-messages-set!
      ) ;assistant-messages
      (tool-calls pi-session-stats-tool-calls pi-session-stats-tool-calls-set!)
      (tool-results pi-session-stats-tool-results pi-session-stats-tool-results-set!)
      (tokens pi-session-stats-tokens)
      (cost pi-session-stats-cost pi-session-stats-cost-set!)
    ) ;define-record-type

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

    (define (%add-usage! stats usage)
      (let ((tokens (pi-session-stats-tokens stats)))
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
        (pi-session-stats-cost-set! stats
          (+ (pi-session-stats-cost stats) (%ref (%ref usage 'cost '()) 'total 0.0))
        ) ;pi-session-stats-cost-set!
      ) ;let
    ) ;define

    (define (%collect-message! stats msg)
      (let ((role (%ref msg 'role "")))
        (cond
         ((equal? role "user")
          (pi-session-stats-user-messages-set! stats
            (+ (pi-session-stats-user-messages stats) 1)
          ) ;pi-session-stats-user-messages-set!
         ) ;
         ((equal? role "assistant")
          (pi-session-stats-assistant-messages-set! stats
            (+ (pi-session-stats-assistant-messages stats) 1)
          ) ;pi-session-stats-assistant-messages-set!
          (pi-session-stats-tool-calls-set! stats
            (+ (pi-session-stats-tool-calls stats)
              (count (lambda (block) (equal? (%ref block 'type "") "toolCall"))
                (%content-list (%ref msg 'content '()))
              ) ;count
            ) ;+
          ) ;pi-session-stats-tool-calls-set!
          (%add-usage! stats (%ref msg 'usage '()))
         ) ;
         ((equal? role "toolResult")
          (pi-session-stats-tool-results-set! stats
            (+ (pi-session-stats-tool-results stats) 1)
          ) ;pi-session-stats-tool-results-set!
         ) ;
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-get-session-stats file-path)
      (let* ((entries (if (and file-path (file-exists? file-path))
                        (pi-read-session-entries file-path)
                        '()
                      ) ;if
             ) ;entries
             (stats (make-pi-session-stats file-path
                      #f
                      #f
                      0
                      0
                      0
                      0
                      0
                      (make-pi-session-token-stats 0 0 0 0)
                      0
                    ) ;make-pi-session-stats
             ) ;stats
            ) ;
        (for-each
          (lambda (entry)
            (let ((type (%ref entry 'type "")))
              (cond ((equal? type "session")
                     (pi-session-stats-session-id-set! stats (%ref entry 'id #f))
                    ) ;
                    ((equal? type "session_info")
                     (pi-session-stats-session-name-set! stats (%ref entry 'name #f))
                    ) ;
                    ((equal? type "message")
                     (pi-session-stats-total-messages-set! stats
                       (+ (pi-session-stats-total-messages stats) 1)
                     ) ;pi-session-stats-total-messages-set!
                     (%collect-message! stats (%ref entry 'message '()))
                    ) ;
              ) ;cond
            ) ;let
          ) ;lambda
          entries
        ) ;for-each
        stats
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
      (let* ((name (pi-session-stats-session-name stats))
             (file (pi-session-stats-session-file stats))
             (id (pi-session-stats-session-id stats))
             (tokens (pi-session-stats-tokens stats))
             (input (pi-session-token-stats-input tokens))
             (output (pi-session-token-stats-output tokens))
             (cache-read (pi-session-token-stats-cache-read tokens))
             (cache-write (pi-session-token-stats-cache-write tokens))
             (prompt-tokens (+ input cache-read cache-write))
             (cost (pi-session-stats-cost stats))
             (lines
               (append (list "Session Info" "")
                 (if name (list (string-append "Name: " name)) '())
                 (list (string-append "File: " (or file "In-memory"))
                   (string-append "ID: " (or id "Unavailable"))
                   ""
                   "Messages"
                   (string-append "Total: "
                     (number->string (pi-session-stats-total-messages stats))
                   ) ;string-append
                   (string-append "User: " (number->string (pi-session-stats-user-messages stats)))
                   (string-append "Assistant: "
                     (number->string (pi-session-stats-assistant-messages stats))
                   ) ;string-append
                   (string-append "Tools: "
                     (number->string (pi-session-stats-tool-calls stats))
                     " calls, "
                     (number->string (pi-session-stats-tool-results stats))
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
