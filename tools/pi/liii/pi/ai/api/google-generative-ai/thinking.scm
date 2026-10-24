;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api google-generative-ai thinking)
  (import (scheme base) (scheme char) (liii string) (liii pi ai message))
  (export gemini-thinking-level->google-level gemini-build-thinking-config
    gemini-inject-thinking-config gemini-is-thinking-part? gemini-part-text
    gemini-part-thought-signature gemini-stream-state-active-type
    gemini-stream-stop-active-block! gemini-append-thinking-block!
  ) ;export
  (begin

    (define (gemini-thinking-level->google-level level)
      (cond ((equal? level "minimal") "MINIMAL")
            ((equal? level "low") "LOW")
            ((equal? level "medium") "MEDIUM")
            ((or (equal? level "high") (equal? level "xhigh") (equal? level "max")) "HIGH")
            (else #f)
      ) ;cond
    ) ;define

    (define (gemini-build-thinking-config level)
      (let ((api-level (and (string? level) (gemini-thinking-level->google-level level))))
        (if api-level
          (list (cons "thinkingConfig"
                  (list (cons "includeThoughts" #t) (cons "thinkingLevel" api-level))
                ) ;cons
          ) ;list
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (gemini-inject-thinking-config req-body level)
      (let ((tc (gemini-build-thinking-config level)))
        (if tc (append req-body (list (cons "generationConfig" tc))) req-body)
      ) ;let
    ) ;define

    (define (gemini-is-thinking-part? part)
      (if (not (pair? part))
        #f
        (let ((pair (or (assoc "thought" part) (assoc 'thought part))))
          (and pair
            (let ((val (cdr pair)))
              (or (eq? val #t) (eq? val 'true) (equal? val "true"))
            ) ;let
          ) ;and
        ) ;let
      ) ;if
    ) ;define

    (define (gemini-part-text part)
      (if (not (pair? part))
        ""
        (let ((p (or (assoc "text" part) (assoc 'text part))))
          (if (and p (string? (cdr p))) (cdr p) "")
        ) ;let
      ) ;if
    ) ;define

    (define (gemini-part-thought-signature part)
      (if (not (pair? part))
        #f
        (let ((p (or (assoc "thoughtSignature" part) (assoc 'thoughtSignature part))))
          (and p (string? (cdr p)) (cdr p))
        ) ;let
      ) ;if
    ) ;define

    (define (gemini-stream-state-active-type stream-state)
      (if (>= (vector-length stream-state) 6) (vector-ref stream-state 5) #f)
    ) ;define

    (define (gemini-stream-stop-active-block! stream-state on-event)
      (let ((act-type (gemini-stream-state-active-type stream-state)))
        (cond
         ((and (equal? act-type "text") (string? (vector-ref stream-state 1)))
          (let ((cur-idx (- (length (vector-ref stream-state 0)) 1))
                (content (vector-ref stream-state 1))
                (usage (vector-ref stream-state 2))
               ) ;
            (vector-set! stream-state 1 #f)
            (vector-set! stream-state 5 #f)
            (when (procedure? on-event)
              (on-event (list (cons 'type "text_end")
                          (cons 'contentIndex cur-idx)
                          (cons 'content content)
                          (cons 'usage usage)
                        ) ;list
              ) ;on-event
            ) ;when
          ) ;let
         ) ;
         ((and (equal? act-type "thinking") (string? (vector-ref stream-state 6)))
          (let ((cur-idx (- (length (vector-ref stream-state 0)) 1))
                (content (vector-ref stream-state 6))
                (usage (vector-ref stream-state 2))
               ) ;
            (vector-set! stream-state 5 #f)
            (vector-set! stream-state 6 #f)
            (vector-set! stream-state 7 #f)
            (when (procedure? on-event)
              (on-event (list (cons 'type "thinking_end")
                          (cons 'contentIndex cur-idx)
                          (cons 'content content)
                          (cons 'usage usage)
                        ) ;list
              ) ;on-event
            ) ;when
          ) ;let
         ) ;
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (gemini-append-thinking-block! stream-state t ts on-event)
      (if (or (not (string? t)) (string-null? t))
        #t
        (let ((act-type (gemini-stream-state-active-type stream-state)))
          (if (not (equal? act-type "thinking"))
            (begin
              (gemini-stream-stop-active-block! stream-state on-event)
              (vector-set! stream-state 5 "thinking")
              (let* ((blocks (vector-ref stream-state 0))
                     (cur-idx (length blocks))
                     (blk (pi-thinking-block t ts))
                     (usage (vector-ref stream-state 2))
                    ) ;
                (vector-set! stream-state 0 (append blocks (list blk)))
                (vector-set! stream-state 6 t)
                (vector-set! stream-state 7 ts)
                (when (procedure? on-event)
                  (on-event (list (cons 'type "thinking_start")
                              (cons 'contentIndex cur-idx)
                              (cons 'usage usage)
                            ) ;list
                  ) ;on-event
                  (on-event (list (cons 'type "thinking_delta")
                              (cons 'contentIndex cur-idx)
                              (cons 'delta t)
                              (cons 'usage usage)
                            ) ;list
                  ) ;on-event
                ) ;when
              ) ;let*
            ) ;begin
            (let* ((cur-t (vector-ref stream-state 6))
                   (new-t (string-append cur-t t))
                   (cur-sig (or ts (vector-ref stream-state 7)))
                   (blocks (vector-ref stream-state 0))
                   (cur-idx (- (length blocks) 1))
                   (usage (vector-ref stream-state 2))
                   (upd-blocks
                     (let loop
                       ((lst blocks) (acc '()))
                       (if (null? (cdr lst))
                         (append (reverse acc) (list (pi-thinking-block new-t cur-sig)))
                         (loop (cdr lst) (cons (car lst) acc))
                       ) ;if
                     ) ;let
                   ) ;upd-blocks
                  ) ;
              (vector-set! stream-state 0 upd-blocks)
              (vector-set! stream-state 6 new-t)
              (vector-set! stream-state 7 cur-sig)
              (when (procedure? on-event)
                (on-event (list (cons 'type "thinking_delta")
                            (cons 'contentIndex cur-idx)
                            (cons 'delta t)
                            (cons 'usage usage)
                          ) ;list
                ) ;on-event
              ) ;when
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
