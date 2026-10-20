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

(define-library (liii tui main-screen driver)
  (export <main-screen> make-main-screen main-screen? screen-terminal
    screen-buffer screen-lines screen-render! screen-refresh! screen-invalidate!
    screen-resize! screen-clear! screen-auto-hide-cursor?
    screen-auto-hide-cursor-set!
  ) ;export
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii error)
    (liii string)
    (liii tui terminal)
    (liii tui component)
    (liii tui main-screen diff)
    (liii tui main-screen buffer)
  ) ;import
  (begin

    (define-record-type <main-screen>
      (%make-main-screen terminal buffer auto-hide-cursor)
      main-screen?
      (terminal %screen-terminal)
      (buffer %screen-buffer)
      (auto-hide-cursor %screen-auto-hide-cursor %screen-auto-hide-cursor-set!)
    ) ;define-record-type

    (define <main-screen> '<main-screen>)

    (define (%check-main-screen screen caller)
      (unless (main-screen? screen)
        (type-error (string-append caller ": expected <main-screen>, got " (object->string screen))
        ) ;type-error
      ) ;unless
    ) ;define

    (define (make-main-screen term . args)
      (unless (terminal? term)
        (type-error (string-append "make-main-screen: expected terminal, got "
                      (object->string term)
                    ) ;string-append
        ) ;type-error
      ) ;unless
      (let ((auto-hide #t))
        (let parse
          ((rest args))
          (cond ((null? rest) #t)
                ((eq? (car rest) :auto-hide-cursor)
                 (if (pair? (cdr rest))
                   (begin
                     (set! auto-hide (cadr rest))
                     (parse (cddr rest))
                   ) ;begin
                   (type-error "make-main-screen: missing value for :auto-hide-cursor")
                 ) ;if
                ) ;
                ((eq? (car rest) :hide-cursor)
                 (if (pair? (cdr rest))
                   (begin
                     (set! auto-hide (cadr rest))
                     (parse (cddr rest))
                   ) ;begin
                   (type-error "make-main-screen: missing value for :hide-cursor")
                 ) ;if
                ) ;
                (else
                  (type-error (string-append "make-main-screen: invalid keyword argument "
                                (object->string (car rest))
                              ) ;string-append
                  ) ;type-error
                ) ;else
          ) ;cond
        ) ;let
        (%make-main-screen term (make-screen-buffer) auto-hide)
      ) ;let
    ) ;define

    (define (screen-terminal screen)
      (%check-main-screen screen "screen-terminal")
      (%screen-terminal screen)
    ) ;define

    (define (screen-buffer screen)
      (%check-main-screen screen "screen-buffer")
      (%screen-buffer screen)
    ) ;define

    (define (screen-lines screen)
      (%check-main-screen screen "screen-lines")
      (screen-buffer-lines (%screen-buffer screen))
    ) ;define

    (define (screen-auto-hide-cursor? screen)
      (%check-main-screen screen "screen-auto-hide-cursor?")
      (%screen-auto-hide-cursor screen)
    ) ;define

    (define (screen-auto-hide-cursor-set! screen val)
      (%check-main-screen screen "screen-auto-hide-cursor-set!")
      (%screen-auto-hide-cursor-set! screen (if val #t #f))
    ) ;define

    (define (%normalize-lines target caller)
      (cond ((component? target) #f)
            ((string? target)
             (if (string-contains? target "\n") (string-split target "\n") (list target))
            ) ;
            ((list? target)
             (let loop
               ((rest target) (acc '()))
               (if (null? rest)
                 (reverse acc)
                 (let ((item (car rest)))
                   (unless (string? item)
                     (type-error (string-append caller ": line must be string, got " (object->string item))
                     ) ;type-error
                   ) ;unless
                   (if (string-contains? item "\n")
                     (loop (cdr rest) (append (reverse (string-split item "\n")) acc))
                     (loop (cdr rest) (cons item acc))
                   ) ;if
                 ) ;let
               ) ;if
             ) ;let
            ) ;
            (else (type-error (string-append caller
                                ": expected component, string, or list of strings, got "
                                (object->string target)
                              ) ;string-append
                  ) ;type-error
            ) ;else
      ) ;cond
    ) ;define

    (define (%parse-cursor opt)
      (and (pair? opt)
        (let ((c (car opt)))
          (cond
           ((pair? c) (cons (car c) (if (pair? (cdr c)) (cadr c) (cdr c))))
           ((and (number? c) (pair? (cdr opt)) (number? (cadr opt))) (cons c (cadr opt)))
           (else #f)
          ) ;cond
        ) ;let
      ) ;and
    ) ;define

    (define (screen-render! screen target . opt-cursor)
      (%check-main-screen screen "screen-render!")
      (let* ((term (%screen-terminal screen))
             (buf (%screen-buffer screen))
             (auto-hide (%screen-auto-hide-cursor screen))
             (lines
               (if (component? target)
                 (let* ((sz (terminal-size term)) (w (car sz)))
                   (component-render target w)
                 ) ;let*
                 (%normalize-lines target "screen-render!")
               ) ;if
             ) ;lines
             (is-dirty (screen-buffer-dirty? buf))
             (old-lines (screen-buffer-lines buf))
             (diff-ops (if is-dirty (screen-dirty-diff old-lines lines) (screen-diff old-lines lines))
             ) ;diff-ops
             (has-changes (or is-dirty (screen-diff-has-changes? diff-ops)))
            ) ;
        (when auto-hide
          (terminal-hide-cursor! term)
        ) ;when
        (when has-changes
          (for-each
            (lambda (op)
              (let ((type (diff-op-type op)) (idx (diff-op-index op)))
                (cond ((eq? type 'keep) #t)
                      ((or (eq? type 'update) (eq? type 'append))
                       (terminal-set-cursor! term 0 idx)
                       (terminal-clear-line term)
                       (terminal-set-cursor! term 0 idx)
                       (terminal-write term (diff-op-text op))
                      ) ;
                      ((eq? type 'clear) (terminal-set-cursor! term 0 idx) (terminal-clear-line term))
                ) ;cond
              ) ;let
            ) ;lambda
            diff-ops
          ) ;for-each
          (terminal-flush term)
        ) ;when
        (screen-buffer-commit! buf lines)
        (let ((cur (%parse-cursor opt-cursor)))
          (cond (cur (terminal-set-cursor! term (car cur) (cdr cur))
                  (terminal-show-cursor! term)
                ) ;cur
                ((not auto-hide) (terminal-show-cursor! term))
          ) ;cond
        ) ;let
        #t
      ) ;let*
    ) ;define

    (define (screen-invalidate! screen)
      (%check-main-screen screen "screen-invalidate!")
      (screen-buffer-invalidate! (%screen-buffer screen))
      #t
    ) ;define

    (define (screen-refresh! screen)
      (%check-main-screen screen "screen-refresh!")
      (let* ((term (%screen-terminal screen))
             (buf (%screen-buffer screen))
             (lines (screen-buffer-lines buf))
            ) ;
        (screen-buffer-invalidate! buf)
        (screen-buffer-prev-lines-set! buf '())
        (terminal-clear term)
        (unless (null? lines)
          (screen-render! screen lines)
        ) ;unless
        #t
      ) ;let*
    ) ;define

    (define (screen-resize! screen cols rows)
      (%check-main-screen screen "screen-resize!")
      (unless (and (integer? cols) (> cols 0))
        (type-error "screen-resize!: expected positive integer cols")
      ) ;unless
      (unless (and (integer? rows) (> rows 0))
        (type-error "screen-resize!: expected positive integer rows")
      ) ;unless
      (let* ((term (%screen-terminal screen))
             (buf (%screen-buffer screen))
             (lines (screen-buffer-lines buf))
            ) ;
        (when (virtual-terminal? term)
          (virtual-terminal-resize! term cols rows)
        ) ;when
        (screen-buffer-invalidate! buf)
        (screen-buffer-prev-lines-set! buf '())
        (terminal-clear term)
        (unless (null? lines)
          (screen-render! screen lines)
        ) ;unless
        #t
      ) ;let*
    ) ;define

    (define (screen-clear! screen)
      (%check-main-screen screen "screen-clear!")
      (let* ((term (%screen-terminal screen)) (buf (%screen-buffer screen)))
        (terminal-clear term)
        (screen-buffer-clear! buf)
        (terminal-flush term)
        #t
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
