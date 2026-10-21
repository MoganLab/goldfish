;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2024-2026 The Goldfish Scheme Authors
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

(define-library (liii tui terminal standard)
  (export make-standard-terminal standard-terminal?)
  (import (liii base)
    (liii error)
    (liii tui terminal protocol)
    (liii tui terminal size)
  ) ;import
  (begin

    (define-record-type <standard-terminal-state>
      (%make-st-state port detect-size-fn cursor-x cursor-y visible? inline? max-y)
      %st-state?
      (port %st-port)
      (detect-size-fn %st-detect-size-fn)
      (cursor-x %st-cursor-x %st-cursor-x-set!)
      (cursor-y %st-cursor-y %st-cursor-y-set!)
      (visible? %st-visible? %st-visible?-set!)
      (inline? %st-inline? %st-inline?-set!)
      (max-y %st-max-y %st-max-y-set!)
    ) ;define-record-type

    (define (%st-write term text)
      (let ((state (terminal-data term)))
        (display text (%st-port state))
      ) ;let
    ) ;define

    (define (%st-get-size term)
      (let ((state (terminal-data term)))
        ((%st-detect-size-fn state))
      ) ;let
    ) ;define

    (define (%st-get-cursor term)
      (let ((state (terminal-data term)))
        (list (%st-cursor-x state) (%st-cursor-y state))
      ) ;let
    ) ;define

    (define (%repeat-crlf n)
      (let loop
        ((count n) (acc ""))
        (if (<= count 0) acc (loop (- count 1) (string-append acc "\r\n")))
      ) ;let
    ) ;define

    (define (%inline-down-seq state y cur-y max-y)
      (if (<= y max-y)
        (string-append "\x1b;[" (number->string (- y cur-y)) "B\r")
        (let* ((down-to-max (max 0 (- max-y cur-y)))
               (new-lines (- y (max cur-y max-y)))
               (down-str (if (> down-to-max 0)
                           (string-append "\x1b;[" (number->string down-to-max) "B\r")
                           ""
                         ) ;if
               ) ;down-str
               (nl-str (%repeat-crlf new-lines))
              ) ;
          (%st-max-y-set! state y)
          (string-append down-str nl-str)
        ) ;let*
      ) ;if
    ) ;define

    (define (%st-inline-seq state x y)
      (let* ((cur-y (%st-cursor-y state))
             (max-y (%st-max-y state))
             (y-seq
               (cond
                ((< y cur-y) (string-append "\x1b;[" (number->string (- cur-y y)) "A\r"))
                ((> y cur-y) (%inline-down-seq state y cur-y max-y))
                (else "\r")
               ) ;cond
             ) ;y-seq
             (x-seq (if (> x 0) (string-append "\x1b;[" (number->string x) "C") ""))
            ) ;
        (string-append y-seq x-seq)
      ) ;let*
    ) ;define

    (define (%st-set-cursor! term x y)
      (let ((state (terminal-data term)))
        (if (%st-inline? state)
          (let ((seq (%st-inline-seq state x y)))
            (%st-cursor-x-set! state x)
            (%st-cursor-y-set! state y)
            (display seq (%st-port state))
          ) ;let
          (begin
            (%st-cursor-x-set! state x)
            (%st-cursor-y-set! state y)
            (display (string-append "\x1b;["
                       (number->string (+ y 1))
                       ";"
                       (number->string (+ x 1))
                       "H"
                     ) ;string-append
              (%st-port state)
            ) ;display
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    (define (%st-clear! term)
      (let ((state (terminal-data term)))
        (%st-cursor-x-set! state 0)
        (%st-cursor-y-set! state 0)
        (display "\x1b;[2J\x1b;[H" (%st-port state))
      ) ;let
    ) ;define

    (define (%st-clear-line! term)
      (let ((state (terminal-data term)))
        (display "\x1b;[2K\r" (%st-port state))
      ) ;let
    ) ;define

    (define (%st-clear-line-to-end! term)
      (let ((state (terminal-data term)))
        (display "\x1b;[K" (%st-port state))
      ) ;let
    ) ;define

    (define (%st-begin-sync! term)
      (let ((state (terminal-data term)))
        (display "\x1b;[?2026h" (%st-port state))
      ) ;let
    ) ;define

    (define (%st-end-sync! term)
      (let ((state (terminal-data term)))
        (display "\x1b;[?2026l" (%st-port state))
      ) ;let
    ) ;define

    (define (%st-get-visible term)
      (let ((state (terminal-data term)))
        (%st-visible? state)
      ) ;let
    ) ;define

    (define (%st-set-visible! term visible?)
      (let ((state (terminal-data term)))
        (let ((target (if visible? #t #f)))
          (unless (eq? (%st-visible? state) target)
            (%st-visible?-set! state target)
            (if target
              (display "\x1b;[?25h" (%st-port state))
              (display "\x1b;[?25l" (%st-port state))
            ) ;if
          ) ;unless
        ) ;let
      ) ;let
    ) ;define

    (define (%st-flush! term)
      (let ((state (terminal-data term)))
        (flush-output-port (%st-port state))
        #t
      ) ;let
    ) ;define

    (define (make-standard-terminal . args)
      (let ((port (current-output-port)) (size-fn terminal-detect-size) (inline #f))
        (let parse
          ((rest args))
          (cond ((null? rest) #t)
                ((eq? (car rest) :output-port)
                 (when (null? (cdr rest))
                   (type-error "make-standard-terminal: missing value for :output-port")
                 ) ;when
                 (set! port (cadr rest))
                 (parse (cddr rest))
                ) ;
                ((eq? (car rest) :detect-size-fn)
                 (when (null? (cdr rest))
                   (type-error "make-standard-terminal: missing value for :detect-size-fn")
                 ) ;when
                 (set! size-fn (cadr rest))
                 (parse (cddr rest))
                ) ;
                ((eq? (car rest) :inline)
                 (when (null? (cdr rest))
                   (type-error "make-standard-terminal: missing value for :inline")
                 ) ;when
                 (set! inline (cadr rest))
                 (parse (cddr rest))
                ) ;
                (else (type-error "make-standard-terminal: invalid keyword arguments"))
          ) ;cond
        ) ;let
        (unless (output-port? port)
          (type-error "make-standard-terminal: :output-port must be an output-port")
        ) ;unless
        (unless (procedure? size-fn)
          (type-error "make-standard-terminal: :detect-size-fn must be a procedure")
        ) ;unless
        (let ((state (%make-st-state port size-fn 0 0 #t (if inline #t #f) 0)))
          (make-terminal-record 'standard
            %st-get-size
            %st-write
            %st-get-cursor
            %st-set-cursor!
            %st-clear!
            %st-clear-line!
            %st-clear-line-to-end!
            %st-begin-sync!
            %st-end-sync!
            %st-get-visible
            %st-set-visible!
            %st-flush!
            state
          ) ;make-terminal-record
        ) ;let
      ) ;let
    ) ;define

    (define (standard-terminal? obj)
      (and (terminal? obj) (eq? (terminal-type obj) 'standard))
    ) ;define

  ) ;begin
) ;define-library
