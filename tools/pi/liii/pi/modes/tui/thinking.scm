;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui thinking)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii error)
    (liii string)
    (liii tui text-width)
    (liii tui box)
    (liii pi modes tui state types)
  ) ;import
  (export <pi-tui-thinking-card> make-pi-tui-thinking-card pi-tui-thinking-card?
    thinking-card-content thinking-card-token-count thinking-card-collapsed?
    thinking-card-completed? thinking-card-scroll-offset thinking-card-toggle!
    thinking-card-append-content! thinking-card-set-completed!
    thinking-card-set-token-count! thinking-card-set-scroll-offset!
    thinking-card-with-toggle render-thinking-collapsed render-thinking-expanded
    render-thinking-card state-find-active-thinking-card
    state-toggle-thinking-card!
  ) ;export
  (begin

    (define-record-type <pi-tui-thinking-card>
      (%make-pi-tui-thinking-card content token-count collapsed? completed?
        scroll-offset
      ) ;%make-pi-tui-thinking-card
      pi-tui-thinking-card?
      (content thinking-card-content %card-set-content!)
      (token-count thinking-card-token-count %card-set-token-count!)
      (collapsed? thinking-card-collapsed? %card-set-collapsed!)
      (completed? thinking-card-completed? %card-set-completed!)
      (scroll-offset thinking-card-scroll-offset %card-set-scroll-offset!)
    ) ;define-record-type

    (define <pi-tui-thinking-card> '<pi-tui-thinking-card>)

    (define (%keyword-value key opts default)
      (let loop
        ((rest opts))
        (cond ((null? rest) default)
              ((and (pair? (cdr rest)) (eq? (car rest) key)) (cadr rest))
              ((null? (cdr rest)) default)
              (else (loop (cddr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%validate-card card caller)
      (unless (pi-tui-thinking-card? card)
        (type-error (string-append caller ": expected <pi-tui-thinking-card>"))
      ) ;unless
    ) ;define

    (define (make-pi-tui-thinking-card . opts)
      (let* ((content (%keyword-value :content opts ""))
             (token-count (%keyword-value :token-count opts 0))
             (collapsed? (%keyword-value :collapsed? opts #t))
             (completed? (%keyword-value :completed? opts #f))
             (scroll-offset (%keyword-value :scroll-offset opts 0))
            ) ;
        (unless (string? content)
          (type-error "make-pi-tui-thinking-card: expected string for :content")
        ) ;unless
        (unless (and (exact-integer? token-count) (>= token-count 0))
          (type-error "make-pi-tui-thinking-card: expected non-negative integer for :token-count"
          ) ;type-error
        ) ;unless
        (unless (boolean? collapsed?)
          (type-error "make-pi-tui-thinking-card: expected boolean for :collapsed?")
        ) ;unless
        (unless (boolean? completed?)
          (type-error "make-pi-tui-thinking-card: expected boolean for :completed?")
        ) ;unless
        (unless (and (exact-integer? scroll-offset) (>= scroll-offset 0))
          (type-error "make-pi-tui-thinking-card: expected non-negative integer for :scroll-offset"
          ) ;type-error
        ) ;unless
        (%make-pi-tui-thinking-card content token-count collapsed? completed?
          scroll-offset
        ) ;%make-pi-tui-thinking-card
      ) ;let*
    ) ;define

    (define (thinking-card-toggle! card)
      (%validate-card card "thinking-card-toggle!")
      (%card-set-collapsed! card (not (thinking-card-collapsed? card)))
      card
    ) ;define

    (define (thinking-card-with-toggle card)
      (%validate-card card "thinking-card-with-toggle")
      (%make-pi-tui-thinking-card (thinking-card-content card)
        (thinking-card-token-count card)
        (not (thinking-card-collapsed? card))
        (thinking-card-completed? card)
        (thinking-card-scroll-offset card)
      ) ;%make-pi-tui-thinking-card
    ) ;define

    (define (thinking-card-append-content! card delta)
      (%validate-card card "thinking-card-append-content!")
      (unless (string? delta)
        (type-error "thinking-card-append-content!: expected string delta")
      ) ;unless
      (%card-set-content! card (string-append (thinking-card-content card) delta))
      card
    ) ;define

    (define (thinking-card-set-completed! card completed?)
      (%validate-card card "thinking-card-set-completed!")
      (unless (boolean? completed?)
        (type-error "thinking-card-set-completed!: expected boolean")
      ) ;unless
      (%card-set-completed! card completed?)
      card
    ) ;define

    (define (thinking-card-set-token-count! card cnt)
      (%validate-card card "thinking-card-set-token-count!")
      (unless (and (exact-integer? cnt) (>= cnt 0))
        (type-error "thinking-card-set-token-count!: expected non-negative integer")
      ) ;unless
      (%card-set-token-count! card cnt)
      card
    ) ;define

    (define (thinking-card-set-scroll-offset! card offset)
      (%validate-card card "thinking-card-set-scroll-offset!")
      (unless (and (exact-integer? offset) (>= offset 0))
        (type-error "thinking-card-set-scroll-offset!: expected non-negative integer")
      ) ;unless
      (%card-set-scroll-offset! card offset)
      card
    ) ;define

    (define (render-thinking-collapsed card width)
      (%validate-card card "render-thinking-collapsed")
      (unless (and (exact-integer? width) (> width 0))
        (type-error "render-thinking-collapsed: expected positive integer width")
      ) ;unless
      (let* ((completed? (thinking-card-completed? card))
             (token-cnt (thinking-card-token-count card))
             (summary (if completed?
                        "> [Thinking] 推理已完成 (按 Tab 展开)"
                        (string-append "> [Thinking] 正在深度思考 ("
                          (number->string token-cnt)
                          " tokens)..."
                        ) ;string-append
                      ) ;if
             ) ;summary
             (styled-summary (string-append "\x1b;[90m" summary "\x1b;[0m"))
            ) ;
        (list (if (> (display-width summary) width)
                (truncate-to-width styled-summary width "")
                styled-summary
              ) ;if
        ) ;list
      ) ;let*
    ) ;define

    (define (%sublist lst start end)
      (let loop
        ((rest lst) (i 0) (acc '()))
        (cond ((or (null? rest) (>= i end)) (reverse acc))
              ((>= i start) (loop (cdr rest) (+ i 1) (cons (car rest) acc)))
              (else (loop (cdr rest) (+ i 1) acc))
        ) ;cond
      ) ;let
    ) ;define

    (define (render-thinking-expanded card width . maybe-max-height)
      (%validate-card card "render-thinking-expanded")
      (unless (and (exact-integer? width) (> width 0))
        (type-error "render-thinking-expanded: expected positive integer width")
      ) ;unless
      (let* ((max-h (if (pair? maybe-max-height) (car maybe-max-height) #f))
             (border (border-style-resolve 'single))
             (title " Thinking (按 Tab 收起) ")
             (top-line (border-render-top border width title))
             (bot-line (border-render-bottom border width))
             (inner-w (max 1 (- width 2)))
             (content (thinking-card-content card))
             (wrapped (wrap-text content inner-w))
             (inner-lines (if (null? wrapped) '("") wrapped))
             (total-inner (length inner-lines))
            ) ;
        (when
          (and max-h (not (and (exact-integer? max-h) (>= max-h 3))))
          (type-error "render-thinking-expanded: expected integer >= 3 for max-height")
        ) ;when
        (let* ((avail-body-h (if max-h (- max-h 2) total-inner))
               (scroll-offset (thinking-card-scroll-offset card))
               (max-scroll (max 0 (- total-inner avail-body-h)))
               (clamped-scroll (min max-scroll scroll-offset))
               (start (- max-scroll clamped-scroll))
               (end (min total-inner (+ start avail-body-h)))
               (visible-inner (%sublist inner-lines start end))
               (boxed-body (map (lambda (line) (border-render-line border line inner-w 0 0)) visible-inner)
               ) ;boxed-body
              ) ;
          (cons top-line (append boxed-body (list bot-line)))
        ) ;let*
      ) ;let*
    ) ;define

    (define (render-thinking-card card width . maybe-max-height)
      (%validate-card card "render-thinking-card")
      (if (thinking-card-collapsed? card)
        (render-thinking-collapsed card width)
        (apply render-thinking-expanded card width maybe-max-height)
      ) ;if
    ) ;define

    (define (state-find-active-thinking-card st)
      (%validate-state st "state-find-active-thinking-card")
      (let loop
        ((rest (reverse (state-messages st))))
        (cond ((null? rest) #f)
              ((pi-tui-thinking-card? (car rest)) (car rest))
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (state-toggle-thinking-card! st)
      (%validate-state st "state-toggle-thinking-card!")
      (let ((card (state-find-active-thinking-card st)))
        (if card
          (begin
            (thinking-card-toggle! card)
            (%state-set-dirty-raw! st #t)
            card
          ) ;begin
          #f
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
