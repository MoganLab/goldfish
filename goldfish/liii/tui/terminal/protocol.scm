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

(define-library (liii tui terminal protocol)
  (export <terminal> make-terminal-record terminal? terminal-size terminal-write
    terminal-cursor terminal-set-cursor! terminal-clear terminal-clear-line
    terminal-cursor-visible? terminal-hide-cursor! terminal-show-cursor!
    terminal-flush terminal-type terminal-data terminal-data-set!
  ) ;export
  (import (liii base) (liii error))
  (begin

    (define-record-type <terminal>
      (%make-terminal type get-size-proc write-proc get-cursor-proc
        set-cursor-proc clear-proc clear-line-proc get-cursor-visible-proc
        set-cursor-visible-proc flush-proc data
      ) ;%make-terminal
      terminal?
      (type terminal-type)
      (get-size-proc %terminal-get-size)
      (write-proc %terminal-write)
      (get-cursor-proc %terminal-get-cursor)
      (set-cursor-proc %terminal-set-cursor)
      (clear-proc %terminal-clear)
      (clear-line-proc %terminal-clear-line)
      (get-cursor-visible-proc %terminal-get-cursor-visible)
      (set-cursor-visible-proc %terminal-set-cursor-visible)
      (flush-proc %terminal-flush)
      (data terminal-data terminal-data-set!)
    ) ;define-record-type

    (define <terminal> '<terminal>)

    (define (make-terminal-record type get-size-proc write-proc get-cursor-proc
              set-cursor-proc clear-proc clear-line-proc get-cursor-visible-proc
              set-cursor-visible-proc flush-proc data
            ) ;make-terminal-record
      (%make-terminal type get-size-proc write-proc get-cursor-proc
        set-cursor-proc clear-proc clear-line-proc get-cursor-visible-proc
        set-cursor-visible-proc flush-proc data
      ) ;%make-terminal
    ) ;define

    (define (%check-terminal term caller)
      (unless (terminal? term)
        (type-error (string-append caller ": expected a terminal, got " (object->string term))
        ) ;type-error
      ) ;unless
    ) ;define

    (define (terminal-size term)
      (%check-terminal term "terminal-size")
      ((%terminal-get-size term) term)
    ) ;define

    (define (terminal-write term text)
      (%check-terminal term "terminal-write")
      (unless (string? text)
        (type-error (string-append "terminal-write: expected string, got " (object->string text))
        ) ;type-error
      ) ;unless
      ((%terminal-write term) term text)
    ) ;define

    (define (terminal-cursor term)
      (%check-terminal term "terminal-cursor")
      ((%terminal-get-cursor term) term)
    ) ;define

    (define (terminal-set-cursor! term x y)
      (%check-terminal term "terminal-set-cursor!")
      (unless (and (integer? x) (exact? x))
        (type-error (string-append "terminal-set-cursor!: expected integer x, got "
                      (object->string x)
                    ) ;string-append
        ) ;type-error
      ) ;unless
      (unless (and (integer? y) (exact? y))
        (type-error (string-append "terminal-set-cursor!: expected integer y, got "
                      (object->string y)
                    ) ;string-append
        ) ;type-error
      ) ;unless
      ((%terminal-set-cursor term) term x y)
    ) ;define

    (define (terminal-clear term)
      (%check-terminal term "terminal-clear")
      ((%terminal-clear term) term)
    ) ;define

    (define (terminal-clear-line term)
      (%check-terminal term "terminal-clear-line")
      ((%terminal-clear-line term) term)
    ) ;define

    (define (terminal-cursor-visible? term)
      (%check-terminal term "terminal-cursor-visible?")
      ((%terminal-get-cursor-visible term) term)
    ) ;define

    (define (terminal-hide-cursor! term)
      (%check-terminal term "terminal-hide-cursor!")
      ((%terminal-set-cursor-visible term) term #f)
    ) ;define

    (define (terminal-show-cursor! term)
      (%check-terminal term "terminal-show-cursor!")
      ((%terminal-set-cursor-visible term) term #t)
    ) ;define

    (define (terminal-flush term)
      (%check-terminal term "terminal-flush")
      ((%terminal-flush term) term)
    ) ;define

  ) ;begin
) ;define-library
