;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui streaming)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii string)
    (liii tui text-width)
    (liii tui terminal)
    (liii tui main-screen)
    (liii pi event types)
    (liii pi modes tui state)
    (liii pi modes tui message)
    (liii pi modes tui viewport)
    (liii pi modes tui layout)
    (liii pi modes tui editor)
  ) ;import
  (export extract-stream-delta append-stream-chunk state-append-stream-chunk!
    state-with-stream-chunk render-tui-screen-lines render-tui-screen!
    stream-screen-render!
  ) ;export
  (begin

    (define (%find-val alist key-sym key-str)
      (if (not (pair? alist))
        #f
        (let ((cell (or (assoc key-sym alist) (assoc key-str alist))))
          (if (pair? cell) (cdr cell) #f)
        ) ;let
      ) ;if
    ) ;define

    (define (%extract-from-pair ev)
      (let ((delta (%find-val ev 'delta "delta"))
            (text (%find-val ev 'text "text"))
            (partial (%find-val ev 'partialResult "partialResult"))
            (inner (%find-val ev 'assistantMessageEvent "assistantMessageEvent"))
           ) ;
        (cond ((string? delta) delta)
              ((string? text) text)
              ((string? partial) partial)
              ((or (pair? inner) (string? inner)) (extract-stream-delta inner))
              (else "")
        ) ;cond
      ) ;let
    ) ;define

    (define (extract-stream-delta ev)
      (cond ((string? ev) ev)
            ((pi-event-message-update? ev)
             (extract-stream-delta (pi-event-message-update-assistant-message-event ev))
            ) ;
            ((pi-event-tool-execution-update? ev)
             (let ((res (pi-event-tool-execution-update-partial-result ev)))
               (if (string? res) res "")
             ) ;let
            ) ;
            ((not (pair? ev)) "")
            (else (%extract-from-pair ev))
      ) ;cond
    ) ;define

    (define (%update-last-assistant-msg rev delta)
      (let* ((last-raw (car rev))
             (last-msg (normalize-pi-tui-message last-raw))
             (rest (cdr rev))
            ) ;
        (if (eq? (message-role last-msg) 'assistant)
          (let ((updated (make-pi-tui-message 'assistant
                           (string-append (message-content last-msg) delta)
                         ) ;make-pi-tui-message
                ) ;updated
               ) ;
            (reverse (cons updated rest))
          ) ;let
          (reverse (cons (make-pi-tui-message 'assistant delta) rev))
        ) ;if
      ) ;let*
    ) ;define

    (define (append-stream-chunk messages chunk)
      (unless (list? messages)
        (type-error "append-stream-chunk: expected list for messages")
      ) ;unless
      (let ((delta (if (string? chunk) chunk (extract-stream-delta chunk))))
        (if (null? messages)
          (list (make-pi-tui-message 'assistant delta))
          (%update-last-assistant-msg (reverse messages) delta)
        ) ;if
      ) ;let
    ) ;define

    (define (state-append-stream-chunk! st chunk)
      (unless (pi-tui-state? st)
        (type-error "state-append-stream-chunk!: expected <pi-tui-state>")
      ) ;unless
      (let ((new-msgs (append-stream-chunk (state-messages st) chunk)))
        (state-set-messages! st new-msgs)
        (state-set-status! st 'streaming)
        (state-set-scroll-offset! st 0)
        (state-mark-dirty! st)
        st
      ) ;let
    ) ;define

    (define (state-with-stream-chunk st chunk)
      (unless (pi-tui-state? st)
        (type-error "state-with-stream-chunk: expected <pi-tui-state>")
      ) ;unless
      (let ((new-msgs (append-stream-chunk (state-messages st) chunk)))
        (state-with-status (state-with-scroll-offset (state-with-messages st new-msgs) 0)
          'streaming
        ) ;state-with-status
      ) ;let
    ) ;define

    (define (%pad-lines lines target-height)
      (let ((len (length lines)))
        (if (>= len target-height)
          (let loop
            ((rest lines) (i 0) (acc '()))
            (if (or (null? rest) (>= i target-height))
              (reverse acc)
              (loop (cdr rest) (+ i 1) (cons (car rest) acc))
            ) ;if
          ) ;let
          (let loop
            ((i len) (acc (reverse lines)))
            (if (>= i target-height) (reverse acc) (loop (+ i 1) (cons "" acc)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%status-tag status)
      (case status
       ((idle) " IDLE ")
       ((running) " RUNNING ")
       ((streaming) " STREAMING ")
       ((confirm) " CONFIRM ")
       ((exit) " EXIT ")
       (else " ")
      ) ;case
    ) ;define

    (define (%compute-scroll-indicator st msg-rect)
      (let* ((msg-w (rect-width msg-rect))
             (msg-h (rect-height msg-rect))
             (rendered-count
               (let loop
                 ((m (state-messages st)) (cnt 0))
                 (if (null? m)
                   cnt
                   (loop (cdr m) (+ cnt (length (render-message-lines (car m) msg-w))))
                 ) ;if
               ) ;let
             ) ;rendered-count
            ) ;
        (viewport-scroll-indicator rendered-count msg-h (state-scroll-offset st))
      ) ;let*
    ) ;define

    (define (%render-status-line st layout)
      (let* ((rect (layout-status-rect layout))
             (w (rect-width rect))
             (h (rect-height rect))
            ) ;
        (if (<= h 0)
          '()
          (let* ((left-text (string-append "[" (%status-tag (state-status st)) "]"))
                 (scroll-ind (%compute-scroll-indicator st (layout-messages-rect layout)))
                 (space-w (max 0 (- w (display-width left-text) (display-width scroll-ind))))
                 (line (string-append left-text (make-string space-w #\space) scroll-ind))
                 (clipped (if (> (display-width line) w) (substring line 0 w) line))
                ) ;
            (list clipped)
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (%compute-msg-lines st msg-w msg-h maybe-committed)
      (cond ((<= msg-h 0) '())
            ((pair? maybe-committed)
             (render-messages-viewport-stream (state-messages st)
               msg-w
               msg-h
               (state-scroll-offset st)
               (car maybe-committed)
             ) ;render-messages-viewport-stream
            ) ;
            (else (render-messages-viewport (state-messages st)
                    msg-w
                    msg-h
                    (state-scroll-offset st)
                  ) ;render-messages-viewport
            ) ;else
      ) ;cond
    ) ;define

    (define (%compute-ed-lines ed ed-w ed-h)
      (cond ((<= ed-h 0) '())
            ((pi-tui-editor? ed) (pi-tui-editor-render-boxed ed ed-w))
            (else '())
      ) ;cond
    ) ;define

    (define (render-tui-screen-lines st layout . maybe-committed-lines)
      (unless (pi-tui-state? st)
        (type-error "render-tui-screen-lines: expected <pi-tui-state>")
      ) ;unless
      (unless (tui-layout? layout)
        (type-error "render-tui-screen-lines: expected <tui-layout>")
      ) ;unless
      (let* ((msg-rect (layout-messages-rect layout))
             (ed-rect (layout-editor-rect layout))
             (msg-h (rect-height msg-rect))
             (msg-w (rect-width msg-rect))
             (msg-lines (%compute-msg-lines st msg-w msg-h maybe-committed-lines))
             (padded-msg-lines (%pad-lines msg-lines msg-h))
             (ed-h (rect-height ed-rect))
             (ed-w (rect-width ed-rect))
             (ed (state-input-editor st))
             (ed-lines (%compute-ed-lines ed ed-w ed-h))
             (padded-ed-lines (%pad-lines ed-lines ed-h))
             (status-lines (%render-status-line st layout))
            ) ;
        (append padded-msg-lines padded-ed-lines status-lines)
      ) ;let*
    ) ;define

    (define (%resolve-screen-layout st maybe-layout)
      (if (and (pair? maybe-layout) (tui-layout? (car maybe-layout)))
        (car maybe-layout)
        (let* ((ed (state-input-editor st))
               (lines (if (pi-tui-editor? ed) (pi-tui-editor-lines ed) 1))
              ) ;
          (compute-tui-layout (state-cols st) (state-rows st) lines)
        ) ;let*
      ) ;if
    ) ;define

    (define (%compute-cursor-pos st ed layout)
      (if
        (not (and (eq? (state-status st) 'idle) (pi-tui-editor? ed)))
        #f
        (let* ((cur (pi-tui-editor-cursor ed))
               (c-col (car cur))
               (c-row (cdr cur))
               (ed-rect (layout-editor-rect layout))
               (top (rect-top ed-rect))
               (left (rect-left ed-rect))
              ) ;
          (cons (+ left c-col 1) (+ top c-row 1))
        ) ;let*
      ) ;if
    ) ;define

    (define (render-tui-screen! screen st . maybe-layout)
      (unless (main-screen? screen)
        (type-error "render-tui-screen!: expected <main-screen>")
      ) ;unless
      (unless (pi-tui-state? st)
        (type-error "render-tui-screen!: expected <pi-tui-state>")
      ) ;unless
      (let* ((layout (%resolve-screen-layout st maybe-layout))
             (lines (render-tui-screen-lines st layout))
             (ed (state-input-editor st))
             (cursor-pos (%compute-cursor-pos st ed layout))
            ) ;
        (if cursor-pos
          (screen-render! screen lines cursor-pos)
          (screen-render! screen lines)
        ) ;if
      ) ;let*
    ) ;define

    (define (stream-screen-render! screen st chunk . maybe-layout)
      (state-append-stream-chunk! st chunk)
      (apply render-tui-screen! screen st maybe-layout)
    ) ;define

  ) ;begin
) ;define-library
