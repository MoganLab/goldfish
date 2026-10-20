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

(define-library (liii tui loader presets)
  (export spinner-presets spinner-frames-resolve spinner-status-icon)
  (import (liii base) (liii error))
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
      (and (pair? lst)
        (let loop
          ((rest lst))
          (cond ((null? rest) #t)
                ((string? (car rest)) (loop (cdr rest)))
                (else #f)
          ) ;cond
        ) ;let
      ) ;and
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
