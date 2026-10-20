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

(define-library (liii pi compass tools)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii pi args)
    (liii pi tools record)
    (liii pi compass types)
    (liii pi compass project)
    (liii pi compass store)
    (liii pi compass render)
    (liii pi compass tour)
  ) ;import
  (export pi-codebase-map-schema pi-code-tour-schema pi-execute-codebase-map
    pi-execute-code-tour pi-codebase-map-definition pi-code-tour-definition
    pi-compass-extend-tool-names
  ) ;export
  (begin

    (define %compass-tools '("codebase_map" "code_tour"))

    (define %guidelines
      '("Use codebase_map to understand the overall project structure. Use code_tour to walk through specific areas in detail.")
    ) ;define

    (define (%object-schema properties)
      (list (cons "type" "object")
        (cons "properties" (if (null? properties) '(()) properties))
      ) ;list
    ) ;define

    (define (%string-property description)
      (list (cons "type" "string") (cons "description" description))
    ) ;define

    (define (pi-codebase-map-schema)
      (%object-schema '())
    ) ;define

    (define (pi-code-tour-schema)
      (%object-schema (list (cons "topic"
                              (%string-property "Tour topic (e.g. 'src', 'testing', 'ci'). Omit to list available topics."
                              ) ;%string-property
                            ) ;cons
                      ) ;list
      ) ;%object-schema
    ) ;define

    (define (%arg args key)
      (let* ((str-key (if (symbol? key) (symbol->string key) key))
             (pair (assoc str-key args))
            ) ;
        (if pair (cdr pair) #f)
      ) ;let*
    ) ;define

    (define (%codemap-for cwd)
      (let* ((project (pi-compass-detect-project cwd))
             (file (pi-compass-default-codemap-path cwd))
             (result (pi-compass-get-or-generate cwd project file #f))
            ) ;
        (pi-compass-jref result "codemap" #f)
      ) ;let*
    ) ;define

    (define (pi-execute-codebase-map args . opts)
      (let ((cwd (path->string (path-cwd))))
        (let ((codemap (%codemap-for cwd)))
          (if codemap
            (pi-tool-result (pi-compass-render-codemap codemap) #f)
            (pi-tool-result "No project detected." #t)
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (%topics-text topics)
      (if (null? topics)
        "No tour topics detected."
        (string-append "Available tour topics: " (string-join topics ", "))
      ) ;if
    ) ;define

    (define (pi-execute-code-tour args . opts)
      (let* ((cwd (path->string (path-cwd)))
             (codemap (%codemap-for cwd))
             (topic (%arg args "topic"))
             (topic (if (%string-not-empty? topic) topic #f))
            ) ;
        (cond ((not codemap) (pi-tool-result "No project detected." #t))
              ((not topic)
               (pi-tool-result (%topics-text (pi-compass-detect-topics cwd codemap)) #f)
              ) ;
              (else (pi-tool-result (pi-compass-render-tour (pi-compass-build-tour cwd topic codemap))
                      #f
                    ) ;pi-tool-result
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-codebase-map-definition)
      (pi-tool-definition "codebase_map"
        "Returns a structured map of the current codebase including directory tree, packages, frameworks, entry points, build scripts, and conventions"
        (pi-codebase-map-schema)
        pi-execute-codebase-map
        "Get or generate a structured map of the current codebase"
        %guidelines
      ) ;pi-tool-definition
    ) ;define

    (define (pi-code-tour-definition)
      (pi-tool-definition "code_tour"
        "Returns a guided walkthrough of a specific area of the codebase, or lists available topics"
        (pi-code-tour-schema)
        pi-execute-code-tour
        "Get a guided code tour for a specific topic or area"
        %guidelines
      ) ;pi-tool-definition
    ) ;define

    (define (%string-not-empty? s)
      (and (string? s) (not (string-null? (string-trim-both s))))
    ) ;define

    (define (%exclude-tools tools-list spec)
      (if (not (%string-not-empty? spec))
        tools-list
        (let* ((raw-items (string-split spec ","))
               (excluded
                 (filter (lambda (s) (not (string-null? s))) (map string-trim-both raw-items))
               ) ;excluded
              ) ;
          (filter (lambda (t) (not (member t excluded))) tools-list)
        ) ;let*
      ) ;if
    ) ;define

    (define (pi-compass-extend-tool-names base parsed)
      (cond ((pi-args-ref parsed 'no-tools) base)
            ((%string-not-empty? (pi-args-ref parsed 'tools)) base)
            (else
              (let* ((extras (%exclude-tools %compass-tools (pi-args-ref parsed 'exclude-tools)))
                     (missing
                       (filter (lambda (name) (not (member name base))) extras)
                     ) ;missing
                    ) ;
                (append base missing)
              ) ;let*
            ) ;else
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
