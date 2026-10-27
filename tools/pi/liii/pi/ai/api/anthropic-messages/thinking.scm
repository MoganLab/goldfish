;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api anthropic-messages thinking)
  (import (scheme base) (scheme char) (liii string) (liii pi ai message))
  (export anthropic-thinking-level->budget anthropic-inject-thinking
    anthropic-start-thinking-block anthropic-handle-thinking-delta
    anthropic-finish-thinking-block
  ) ;export
  (begin

    (define (anthropic-thinking-level->budget level)
      (cond ((equal? level "minimal") 1024)
            ((equal? level "low") 2048)
            ((equal? level "medium") 8192)
            ((equal? level "high") 16384)
            ((or (equal? level "xhigh") (equal? level "max")) 16384)
            (else #f)
      ) ;cond
    ) ;define

    (define (anthropic-inject-thinking req-body level)
      (let ((budget (anthropic-thinking-level->budget level)))
        (if budget
          (let* ((th-val (list (cons "type" "enabled") (cons "budget_tokens" budget)))
                 (with-th (cons (cons "thinking" th-val) req-body))
                 (cur-max (or (assoc "max_tokens" req-body) (assoc 'max_tokens req-body)))
                 (eff-max (if cur-max (cdr cur-max) 8192))
                ) ;
            (if (<= eff-max budget)
              (map
                (lambda (pair)
                  (if (or (equal? (car pair) "max_tokens") (eq? (car pair) 'max_tokens))
                    (cons (car pair) (+ budget 4096))
                    pair
                  ) ;if
                ) ;lambda
                with-th
              ) ;map
              with-th
            ) ;if
          ) ;let*
          req-body
        ) ;if
      ) ;let
    ) ;define

    (define (anthropic-start-thinking-block stream-state idx on-ev)
      (vector-set! stream-state
        6
        (list (cons 'thinking "") (cons 'signature "") (cons 'index idx))
      ) ;vector-set!
      (vector-set! stream-state 1 #f)
      (when (procedure? on-ev)
        (on-ev (list (cons 'type "thinking_start")
                 (cons 'contentIndex idx)
                 (cons 'usage (vector-ref stream-state 2))
               ) ;list
        ) ;on-ev
      ) ;when
    ) ;define

    (define (anthropic-handle-thinking-delta delta idx stream-state on-ev)
      (let* ((th (cdr (or (assoc "thinking" delta) '(#f . ""))))
             (sig (cdr (or (assoc "signature" delta) '(#f . ""))))
             (cur-th (vector-ref stream-state 6))
            ) ;
        (when (pair? cur-th)
          (let* ((prev-text (cdr (assoc 'thinking cur-th)))
                 (prev-sig (cdr (assoc 'signature cur-th)))
                 (new-text (string-append prev-text th))
                 (new-sig (string-append prev-sig sig))
                 (upd (list (cons 'thinking new-text) (cons 'signature new-sig) (cons 'index idx))
                 ) ;upd
                ) ;
            (vector-set! stream-state 6 upd)
            (when (and (procedure? on-ev) (not (string-null? th)))
              (on-ev (list (cons 'type "thinking_delta")
                       (cons 'contentIndex idx)
                       (cons 'delta th)
                       (cons 'usage (vector-ref stream-state 2))
                     ) ;list
              ) ;on-ev
            ) ;when
          ) ;let*
        ) ;when
      ) ;let*
    ) ;define

    (define (anthropic-finish-thinking-block stream-state on-ev)
      (let* ((cur-th (vector-ref stream-state 6))
             (blocks (vector-ref stream-state 0))
             (usage (vector-ref stream-state 2))
            ) ;
        (when (pair? cur-th)
          (let* ((th (cdr (assoc 'thinking cur-th)))
                 (sig (cdr (assoc 'signature cur-th)))
                 (eff-sig (if (string-null? sig) #f sig))
                 (blk (pi-thinking-block th eff-sig))
                ) ;
            (vector-set! stream-state 0 (append blocks (list blk)))
            (vector-set! stream-state 6 #f)
            (when (procedure? on-ev)
              (on-ev
                (list (cons 'type "thinking_end")
                  (cons 'contentIndex (cdr (assoc 'index cur-th)))
                  (cons 'content th)
                  (cons 'usage usage)
                ) ;list
              ) ;on-ev
            ) ;when
          ) ;let*
        ) ;when
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
