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

(define-library (liii tui loader presets)
  (export spinner-presets spinner-frames-resolve spinner-status-icon)
  (import (liii base) (liii error) (liii list))
  (begin

    (define %dots-frames
      '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
    ) ;define

    (define %line-frames '("-" "\\" "|" "/"))

    (define %pipe-frames '("┤" "┘" "┴" "└" "├" "┌" "┬" "┐"))

    (define %star-frames '("✶" "✸" "✹" "✺" "✹" "✷"))

    (define %bounce-frames '("⠁" "⠂" "⠄" "⠂"))

    (define %pulse-frames '("█" "▓" "▒" "░" "▒" "▓"))

    (define %ascii-frames '("|" "/" "-" "\\"))

    (define (%preset-map)
      `((dots . ,%dots-frames)
        (line . ,%line-frames)
        (pipe . ,%pipe-frames)
        (star . ,%star-frames)
        (bounce . ,%bounce-frames)
        (pulse . ,%pulse-frames)
        (ascii . ,%ascii-frames))
    ) ;define

    (define (spinner-presets)
      '(dots line pipe star bounce pulse ascii)
    ) ;define

    (define (%string-list? lst)
      (and (pair? lst) (every string? lst))
    ) ;define

    (define (spinner-frames-resolve spec)
      (cond
       ((symbol? spec)
        (let ((pair (assq spec (%preset-map))))
          (if pair
            (cdr pair)
            (type-error "spinner-frames-resolve: unknown preset style" spec)
          ) ;if
        ) ;let
       ) ;
       ((list? spec)
        (if (%string-list? spec)
          spec
          (type-error "spinner-frames-resolve: frames list must be non-empty strings"
            spec
          ) ;type-error
        ) ;if
       ) ;
       (else (type-error "spinner-frames-resolve: expected preset symbol or string list"
               spec
             ) ;type-error
       ) ;else
      ) ;cond
    ) ;define

    (define (spinner-status-icon status)
      (case status
       ((spinning) #f)
       ((success) "✔")
       ((error) "✖")
       ((warning) "⚠")
       ((stopped) "■")
       (else (type-error "spinner-status-icon: invalid status symbol" status))
      ) ;case
    ) ;define

  ) ;begin
) ;define-library
