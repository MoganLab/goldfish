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
