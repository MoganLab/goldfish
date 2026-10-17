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

(define-library (liii pi)
  (import (scheme base)
    (liii string)
    (liii pi truncate)
    (liii pi read)
    (liii pi write)
    (liii pi edit)
    (liii pi bash)
    (liii pi ls)
    (liii pi find)
    (liii pi grep)
    (liii pi config)
  ) ;import
  (export resolve-app-mode format-file-block build-initial-prompt
    PI_DEFAULT_MAX_LINES PI_DEFAULT_MAX_BYTES PI_GREP_MAX_LINE_LENGTH
    pi-format-size pi-truncate-line pi-truncate-head pi-truncate-tail pi-arg-ref
    pi-resolve-path pi-read-schema pi-execute-read pi-write-schema
    pi-execute-write pi-edit-schema pi-execute-edit pi-bash-schema
    pi-execute-bash pi-ls-schema pi-execute-ls pi-find-schema pi-execute-find
    pi-grep-schema pi-execute-grep pi-resolve-config-value json-dict-ref
    pi-get-agent-dir pi-read-json-file pi-load-settings pi-resolve-proxy-url
  ) ;export
  (begin

    (define (resolve-app-mode mode-option print-flag stdin-tty stdout-tty)
      (cond ((or (equal? mode-option "json") (eq? mode-option 'json)) 'json)
            ((or print-flag (not stdin-tty) (not stdout-tty)) 'print)
            (else 'print)
      ) ;cond
    ) ;define

    (define (format-file-block abs-path content)
      (string-append "<file name=\"" abs-path "\">\n" content "\n</file>\n")
    ) ;define

    (define (build-initial-prompt stdin-content file-text messages)
      (let* ((has-stdin? (and (string? stdin-content) (not (string-null? stdin-content))))
             (has-file? (and (string? file-text) (not (string-null? file-text))))
             (has-msg? (and (pair? messages) (string? (car messages))))
             (first-msg (if has-msg? (car messages) #f))
             (rest-msgs (if has-msg? (cdr messages) '()))
             (parts (append (if has-stdin? (list stdin-content) '())
                      (if has-file? (list file-text) '())
                      (if first-msg (list first-msg) '())
                    ) ;append
             ) ;parts
             (initial-message (if (null? parts) "" (apply string-append parts)))
            ) ;
        (values initial-message rest-msgs)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
