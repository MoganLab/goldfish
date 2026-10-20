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

(define-library (liii pi modes compass)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii pi args)
    (liii pi compass args)
    (liii pi compass types)
    (liii pi compass project)
    (liii pi compass store)
    (liii pi compass render)
    (liii pi compass tour)
    (liii pi compass tools)
    (liii pi modes output)
  ) ;import
  (export run-compass-flow pi-compass-augment-config pi-compass-extend-tool-names)
  (begin

    (define (%replace-field key value cfg)
      (cons (cons key value) (filter (lambda (pair) (not (eq? (car pair) key))) cfg))
    ) ;define

    (define (pi-compass-augment-config parsed cfg)
      (%replace-field 'tools
        (pi-compass-extend-tool-names (cdr (assoc 'tools cfg)) parsed)
        cfg
      ) ;%replace-field
    ) ;define

    (define (%out mode text)
      (if (eq? mode 'json) (display-stderr text) (begin (display text) (newline)))
    ) ;define

    (define (%codemap-path parsed cwd)
      (let ((raw (pi-args-ref parsed 'codemap-file)))
        (if
          (and (string? raw) (not (string-null? (string-trim-both raw))))
          (pi-compass-resolve-path (string-trim-both raw) cwd)
          (pi-compass-default-codemap-path cwd)
        ) ;if
      ) ;let
    ) ;define

    (define (%load-codemap parsed cwd refresh?)
      (let* ((project (pi-compass-detect-project cwd))
             (file (%codemap-path parsed cwd))
             (result (pi-compass-get-or-generate cwd project file refresh?))
            ) ;
        (pi-compass-jref result "codemap" #f)
      ) ;let*
    ) ;define

    (define (%topics-text topics)
      (if (null? topics)
        "No tour topics detected in this project."
        (string-append "Available tour topics:\n"
          (string-join (map (lambda (t) (string-append "  - " t)) topics) "\n")
          "\n\nUsage: bin/gf pi --tour <topic>"
        ) ;string-append
      ) ;if
    ) ;define

    (define (run-codemap-flow parsed mode)
      (let* ((cwd (path->string (path-cwd)))
             (codemap (%load-codemap parsed cwd (pi-args-ref parsed 'refresh)))
            ) ;
        (if codemap
          (begin
            (%out mode (pi-compass-render-codemap codemap))
            0
          ) ;begin
          (begin
            (%out mode "No project detected.")
            1
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    (define (run-tour-flow parsed mode)
      (let* ((cwd (path->string (path-cwd)))
             (codemap (%load-codemap parsed cwd #f))
             (topic (pi-compass-tour-topic-arg parsed))
            ) ;
        (cond ((not codemap) (begin (%out mode "No project detected.") 1))
              ((not topic)
               (begin
                 (%out mode (%topics-text (pi-compass-detect-topics cwd codemap)))
                 0
               ) ;begin
              ) ;
              (else
                (begin
                  (%out mode (pi-compass-render-tour (pi-compass-build-tour cwd topic codemap)))
                  0
                ) ;begin
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (run-compass-flow parsed mode)
      (cond ((pi-args-ref parsed 'codemap) (run-codemap-flow parsed mode))
            ((pi-args-ref parsed 'tour) (run-tour-flow parsed mode))
            (else 0)
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
