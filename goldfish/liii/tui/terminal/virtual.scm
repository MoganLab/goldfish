;;
;; Copyright (C) 2024 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii tui terminal virtual)
  (export make-virtual-terminal virtual-terminal? virtual-terminal-screen
    virtual-terminal-raw virtual-terminal-resize!
  ) ;export
  (import (liii base)
    (liii error)
    (liii tui terminal protocol)
    (liii tui terminal size)
    (liii tui terminal virtual state)
  ) ;import
  (begin

    (define (%advance-cursor-line! state)
      (vt-cursor-x-set! state 0)
      (let ((ny (+ (vt-cursor-y state) 1)))
        (if (>= ny (vt-height state))
          (begin
            (scroll-up-one-line! state)
            (vt-cursor-y-set! state (- (vt-height state) 1))
          ) ;begin
          (vt-cursor-y-set! state ny)
        ) ;if
      ) ;let
    ) ;define

    (define (%put-char! state ch)
      (let ((cx (vt-cursor-x state))
            (cy (vt-cursor-y state))
            (w (vt-width state))
            (h (vt-height state))
           ) ;
        (when (and (< cx w) (< cy h))
          (let ((row (vector-ref (vt-lines state) cy)))
            (vector-set! row cx ch)
          ) ;let
        ) ;when
        (vt-cursor-x-set! state (+ cx 1))
      ) ;let
    ) ;define

    (define (%vt-write! term text)
      (let* ((state (terminal-data term)) (len (string-length text)))
        (vt-raw-set! state (append (vt-raw state) (list text)))
        (let loop
          ((i 0))
          (when (< i len)
            (let ((ch (string-ref text i)))
              (cond
               ((char=? ch #\return)
                (vt-cursor-x-set! state 0)
                (when
                  (and (< (+ i 1) len) (char=? (string-ref text (+ i 1)) #\newline))
                  (set! i (+ i 1))
                  (%advance-cursor-line! state)
                ) ;when
               ) ;
               ((char=? ch #\newline) (%advance-cursor-line! state))
               (else (%put-char! state ch))
              ) ;cond
            ) ;let
            (loop (+ i 1))
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
        (vector-set! (vt-lines state) cy (make-char-row w))
      ) ;let*
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
        (let ((state (make-vt-state w h 0 0 #t (make-screen-lines w h) '())))
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
            (lambda (term) (vt-visible? (terminal-data term)))
            (lambda (term v) (vt-visible?-set! (terminal-data term) v))
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
      (let* ((state (terminal-data vt)) (h (vt-height state)) (lines (vt-lines state)))
        (let loop
          ((i 0) (res '()))
          (if (< i h)
            (let* ((row (vector-ref lines i)) (str (vector->string row)))
              (loop (+ i 1) (cons str res))
            ) ;let*
            (reverse res)
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (virtual-terminal-raw vt)
      (unless (virtual-terminal? vt)
        (type-error "virtual-terminal-raw: expected virtual-terminal")
      ) ;unless
      (vt-raw (terminal-data vt))
    ) ;define

    (define (%copy-row-chars! old-row new-row copy-len)
      (let loop
        ((j 0))
        (when (< j copy-len)
          (vector-set! new-row j (vector-ref old-row j))
          (loop (+ j 1))
        ) ;when
      ) ;let
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
             (new-lines (make-vector new-height))
            ) ;
        (let row-loop
          ((i 0))
          (when (< i new-height)
            (let ((new-row (make-char-row new-width)))
              (when (< i old-h)
                (let ((old-row (vector-ref old-lines i)) (copy-len (min old-w new-width)))
                  (%copy-row-chars! old-row new-row copy-len)
                ) ;let
              ) ;when
              (vector-set! new-lines i new-row)
              (row-loop (+ i 1))
            ) ;let
          ) ;when
        ) ;let
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
