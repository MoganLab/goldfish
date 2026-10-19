;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii pi compact flow)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii pi args)
    (liii pi config)
    (liii pi session)
    (liii pi compact tokens)
    (liii pi compact session)
  ) ;import
  (export pi-format-compaction-summary pi-make-compaction-option)
  (begin

    (define (%display-stderr msg)
      (let ((p (if (defined? '*stderr*) *stderr* (current-error-port))))
        (display msg p)
        (newline p)
      ) ;let
    ) ;define

    (define (pi-format-compaction-summary result . opt-label)
      (let ((label
              (if (and (pair? opt-label) (string? (car opt-label)))
                (car opt-label)
                "Compacted"
              ) ;if
            ) ;label
           ) ;
        (string-append label
          " session: summarized "
          (number->string (cdr (assoc 'summarized-count result)))
          " messages, kept "
          (number->string (cdr (assoc 'kept-count result)))
          ", tokens before "
          (number->string (cdr (assoc 'tokens-before result)))
        ) ;string-append
      ) ;let
    ) ;define

    (define (%context-window cfg)
      (let* ((meta (cdr (or (assoc 'model-meta cfg) '(#f))))
             (pair (assoc 'context-window meta))
            ) ;
        (if pair (cdr pair) #f)
      ) ;let*
    ) ;define

    (define (%auto-compact cfg sfile state timeout settings window msgs)
      (let* ((estimate (pi-context-token-estimate msgs))
             (tokens (cdr (assoc 'tokens estimate)))
            ) ;
        (if (not (pi-should-compact? tokens window settings))
          msgs
          (begin
            (persist-new-messages! sfile (vector-ref state 0) msgs (vector-ref state 1))
            (vector-set! state 0 msgs)
            (vector-set! state 1 (length msgs))
            (let* ((result (pi-compact-session! cfg sfile settings timeout "auto"))
                   (projection (pi-project-session-messages (pi-read-session-entries sfile)))
                  ) ;
              (%display-stderr (pi-format-compaction-summary result "Auto-compacted"))
              (vector-set! state 0 projection)
              (vector-set! state 1 (length projection))
              projection
            ) ;let*
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-make-compaction-option parsed cfg sfile state timeout)
      (let* ((agent-dir (cdr (assoc 'agent-dir cfg)))
             (settings (pi-load-compaction-settings agent-dir))
             (enabled
               (and sfile
                 (not (pi-args-ref parsed 'no-auto-compact))
                 (eq? #t (cdr (assoc 'enabled settings)))
               ) ;and
             ) ;enabled
             (window (%context-window cfg))
            ) ;
        (and enabled
          (list (cons 'compactor
                  (lambda (msgs) (%auto-compact cfg sfile state timeout settings window msgs))
                ) ;cons
          ) ;list
        ) ;and
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library
