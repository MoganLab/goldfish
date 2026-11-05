;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api anthropic-messages thinking)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii pi ai message)
    (liii pi ai api anthropic-messages stream-state)
  ) ;import
  (export anthropic-thinking-level->budget anthropic-inject-thinking
    anthropic-start-thinking-block anthropic-handle-thinking-delta
    anthropic-finish-thinking-block
  ) ;export
  (begin

    (define-record-type <anthropic-thinking-state>
      (%make-anthropic-thinking-state thinking signature index)
      %anthropic-thinking-state?
      (thinking %anthropic-thinking-state-record-thinking)
      (signature %anthropic-thinking-state-record-signature)
      (index %anthropic-thinking-state-record-index)
    ) ;define-record-type

    (define (%anthropic-thinking-state-thinking state)
      (if (%anthropic-thinking-state? state)
        (%anthropic-thinking-state-record-thinking state)
        (cdr (assoc 'thinking state))
      ) ;if
    ) ;define

    (define (%anthropic-thinking-state-signature state)
      (if (%anthropic-thinking-state? state)
        (%anthropic-thinking-state-record-signature state)
        (cdr (assoc 'signature state))
      ) ;if
    ) ;define

    (define (%anthropic-thinking-state-index state)
      (if (%anthropic-thinking-state? state)
        (%anthropic-thinking-state-record-index state)
        (cdr (assoc 'index state))
      ) ;if
    ) ;define

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
      (anthropic-stream-state-current-thinking-set! stream-state
        (%make-anthropic-thinking-state "" "" idx)
      ) ;anthropic-stream-state-current-thinking-set!
      (anthropic-stream-state-last-text-set! stream-state #f)
      (when (procedure? on-ev)
        (on-ev (list (cons 'type "thinking_start")
                 (cons 'contentIndex idx)
                 (cons 'usage (anthropic-stream-state-usage stream-state))
               ) ;list
        ) ;on-ev
      ) ;when
    ) ;define

    (define (anthropic-handle-thinking-delta delta idx stream-state on-ev)
      (let* ((th (cdr (or (assoc "thinking" delta) '(#f . ""))))
             (sig (cdr (or (assoc "signature" delta) '(#f . ""))))
             (cur-th (anthropic-stream-state-current-thinking stream-state))
            ) ;
        (when cur-th
          (let* ((prev-text (%anthropic-thinking-state-thinking cur-th))
                 (prev-sig (%anthropic-thinking-state-signature cur-th))
                 (new-text (string-append prev-text th))
                 (new-sig (string-append prev-sig sig))
                 (upd (%make-anthropic-thinking-state new-text new-sig idx))
                ) ;
            (anthropic-stream-state-current-thinking-set! stream-state upd)
            (when (and (procedure? on-ev) (not (string-null? th)))
              (on-ev (list (cons 'type "thinking_delta")
                       (cons 'contentIndex idx)
                       (cons 'delta th)
                       (cons 'usage (anthropic-stream-state-usage stream-state))
                     ) ;list
              ) ;on-ev
            ) ;when
          ) ;let*
        ) ;when
      ) ;let*
    ) ;define

    (define (anthropic-finish-thinking-block stream-state on-ev)
      (let* ((cur-th (anthropic-stream-state-current-thinking stream-state))
             (blocks (anthropic-stream-state-blocks stream-state))
             (usage (anthropic-stream-state-usage stream-state))
            ) ;
        (when cur-th
          (let* ((th (%anthropic-thinking-state-thinking cur-th))
                 (sig (%anthropic-thinking-state-signature cur-th))
                 (eff-sig (if (string-null? sig) #f sig))
                 (blk (pi-thinking-block th eff-sig))
                ) ;
            (anthropic-stream-state-blocks-set! stream-state (append blocks (list blk)))
            (anthropic-stream-state-current-thinking-set! stream-state #f)
            (when (procedure? on-ev)
              (on-ev (list (cons 'type "thinking_end")
                       (cons 'contentIndex (%anthropic-thinking-state-index cur-th))
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
