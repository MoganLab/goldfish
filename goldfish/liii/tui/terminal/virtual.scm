;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2026 The Goldfish Scheme Authors
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

(define-library (liii tui terminal virtual)
  (export make-virtual-terminal virtual-terminal? virtual-terminal-screen
    virtual-terminal-raw virtual-terminal-resize! virtual-terminal-push-key!
    virtual-terminal-pop-key!
  ) ;export
  (import (liii base)
    (liii error)
    (liii vector)
    (liii tui terminal protocol)
    (liii tui terminal size)
    (liii tui terminal virtual state)
  ) ;import
  (begin

    (define (%vt-write! term text)
      (let* ((state (terminal-data term)) (len (string-length text)))
        (vt-raw-set! state (append (vt-raw state) (list text)))
        (let loop
          ((i 0))
          (when (< i len)
            (let ((next-i (or (%skip-ansi-csi text i len) (%vt-handle-char! state text i len))))
              (loop next-i)
            ) ;let
          ) ;when
        ) ;let
      ) ;let*
    ) ;define

    (define (%vt-set-cursor! term x y)
      (let* ((state (terminal-data term))
             (w (vt-width state))
             (h (vt-height state))
             (clamped-x (max 0 (min (- w 1) x)))
             (clamped-y (max 0 (min (- h 1) y)))
            ) ;
        (vt-cursor-x-set! state clamped-x)
        (vt-cursor-y-set! state clamped-y)
      ) ;let*
    ) ;define

    (define (%vt-clear! term)
      (let* ((state (terminal-data term)) (w (vt-width state)) (h (vt-height state)))
        (vt-lines-set! state (make-screen-lines w h))
        (vt-cursor-x-set! state 0)
        (vt-cursor-y-set! state 0)
      ) ;let*
    ) ;define

    (define (%vt-clear-line! term)
      (let* ((state (terminal-data term)) (w (vt-width state)) (cy (vt-cursor-y state)))
        (vt-raw-set! state (append (vt-raw state) (list "\x1b;[2K\r")))
        (vector-set! (vt-lines state) cy (make-char-row w))
      ) ;let*
    ) ;define

    (define (%vt-clear-line-to-end! term)
      (let* ((state (terminal-data term))
             (w (vt-width state))
             (cy (vt-cursor-y state))
             (cx (vt-cursor-x state))
             (lines (vt-lines state))
            ) ;
        (vt-raw-set! state (append (vt-raw state) (list "\x1b;[K")))
        (when (and (< cy (vt-height state)) (< cx w))
          (let ((row (vector-ref lines cy)))
            (let loop
              ((x (max 0 cx)))
              (when (< x w)
                (vector-set! row x #\space)
                (loop (+ x 1))
              ) ;when
            ) ;let
          ) ;let
        ) ;when
      ) ;let*
    ) ;define

    (define (%vt-begin-sync! term)
      (let ((state (terminal-data term)))
        (vt-raw-set! state (append (vt-raw state) (list "\x1b;[?2026h")))
      ) ;let
    ) ;define

    (define (%vt-end-sync! term)
      (let ((state (terminal-data term)))
        (vt-raw-set! state (append (vt-raw state) (list "\x1b;[?2026l")))
      ) ;let
    ) ;define

    (define (make-virtual-terminal . args)
      (let ((w #f) (h #f))
        (let parse
          ((rest args))
          (cond ((null? rest) #t)
                ((eq? (car rest) :width)
                 (when (null? (cdr rest))
                   (type-error "make-virtual-terminal: missing value for :width")
                 ) ;when
                 (set! w (cadr rest))
                 (parse (cddr rest))
                ) ;
                ((eq? (car rest) :height)
                 (when (null? (cdr rest))
                   (type-error "make-virtual-terminal: missing value for :height")
                 ) ;when
                 (set! h (cadr rest))
                 (parse (cddr rest))
                ) ;
                ((and (null? (cdr rest)) (not w)) (set! w (car rest)))
                ((and (pair? (cdr rest)) (null? (cddr rest)) (not w) (not h))
                 (set! w (car rest))
                 (set! h (cadr rest))
                ) ;
                (else (type-error "make-virtual-terminal: invalid arguments"))
          ) ;cond
        ) ;let
        (unless w
          (let ((sz (terminal-detect-size)))
            (set! w (car sz))
            (unless h
              (set! h (cadr sz))
            ) ;unless
          ) ;let
        ) ;unless
        (unless h
          (let ((sz (terminal-detect-size)))
            (set! h (cadr sz))
          ) ;let
        ) ;unless
        (check-dimension w "make-virtual-terminal width")
        (check-dimension h "make-virtual-terminal height")
        (let ((state (make-vt-state w h 0 0 #t (make-screen-lines w h) '() '())))
          (make-terminal-record 'virtual
            (lambda (term)
              (let ((st (terminal-data term)))
                (list (vt-width st) (vt-height st))
              ) ;let
            ) ;lambda
            %vt-write!
            (lambda (term)
              (let ((st (terminal-data term)))
                (list (vt-cursor-x st) (vt-cursor-y st))
              ) ;let
            ) ;lambda
            %vt-set-cursor!
            %vt-clear!
            %vt-clear-line!
            %vt-clear-line-to-end!
            %vt-begin-sync!
            %vt-end-sync!
            (lambda (term) (vt-visible? (terminal-data term)))
            (lambda (term v) (vt-visible?-set! (terminal-data term) (if v #t #f)))
            (lambda (term) #t)
            state
          ) ;make-terminal-record
        ) ;let
      ) ;let
    ) ;define

    (define (virtual-terminal? obj)
      (and (terminal? obj) (eq? (terminal-type obj) 'virtual))
    ) ;define

    (define (virtual-terminal-screen vt)
      (unless (virtual-terminal? vt)
        (type-error "virtual-terminal-screen: expected virtual-terminal")
      ) ;unless
      (let ((lines (vt-lines (terminal-data vt))))
        (map vector->string (vector->list lines))
      ) ;let
    ) ;define

    (define (virtual-terminal-push-key! vt key)
      (unless (virtual-terminal? vt)
        (type-error "virtual-terminal-push-key!: expected virtual-terminal")
      ) ;unless
      (let ((state (terminal-data vt)))
        (vt-keys-set! state (append (vt-keys state) (list key)))
      ) ;let
    ) ;define

    (define (virtual-terminal-pop-key! vt)
      (unless (virtual-terminal? vt)
        (type-error "virtual-terminal-pop-key!: expected virtual-terminal")
      ) ;unless
      (let* ((state (terminal-data vt)) (q (vt-keys state)))
        (if (null? q) #f (begin (vt-keys-set! state (cdr q)) (car q)))
      ) ;let*
    ) ;define

    (define (virtual-terminal-raw vt)
      (unless (virtual-terminal? vt)
        (type-error "virtual-terminal-raw: expected virtual-terminal")
      ) ;unless
      (vt-raw (terminal-data vt))
    ) ;define

    (define (virtual-terminal-resize! vt new-width new-height)
      (unless (virtual-terminal? vt)
        (type-error "virtual-terminal-resize!: expected virtual-terminal")
      ) ;unless
      (check-dimension new-width "virtual-terminal-resize! new-width")
      (check-dimension new-height "virtual-terminal-resize! new-height")
      (let* ((state (terminal-data vt))
             (old-w (vt-width state))
             (old-h (vt-height state))
             (old-lines (vt-lines state))
             (new-lines
               (vector-unfold
                 (lambda (i)
                   (let ((new-row (make-char-row new-width)))
                     (when (< i old-h)
                       (let ((old-row (vector-ref old-lines i)) (copy-len (min old-w new-width)))
                         (copy-row-chars! old-row new-row copy-len)
                       ) ;let
                     ) ;when
                     new-row
                   ) ;let
                 ) ;lambda
                 new-height
               ) ;vector-unfold
             ) ;new-lines
            ) ;
        (vt-width-set! state new-width)
        (vt-height-set! state new-height)
        (vt-lines-set! state new-lines)
        ;; 重新钳位光标
        (let ((cx (vt-cursor-x state)) (cy (vt-cursor-y state)))
          (vt-cursor-x-set! state (max 0 (min (- new-width 1) cx)))
          (vt-cursor-y-set! state (max 0 (min (- new-height 1) cy)))
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
