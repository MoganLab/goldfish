;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compass tour)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii path)
    (liii pi time)
    (liii pi compass types)
    (liii pi compass tour files)
  ) ;import
  (export PI_COMPASS_TEST_INDICATORS PI_COMPASS_CI_INDICATORS
    PI_COMPASS_DB_INDICATORS PI_COMPASS_SOURCE_EXTENSIONS
    pi-compass-detect-topics pi-compass-build-tour pi-compass-render-tour
    pi-compass-describe-file
  ) ;export
  (begin

    (define (%top-dir-names codemap)
      (let loop
        ((entries (pi-compass-codemap-tree codemap)) (acc '()))
        (if (null? entries)
          (reverse acc)
          (let ((entry (car entries)))
            (loop (cdr entries)
              (if (pi-compass-entry-dir? entry) (cons (pi-compass-entry-name entry) acc) acc)
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%contains-any? names indicators)
      (any (lambda (i) (member i names)) indicators)
    ) ;define

    (define (%key-paths-mention-ci? key-paths)
      (any
        (lambda (k) (any (lambda (c) (string-contains? k c)) PI_COMPASS_CI_INDICATORS))
        key-paths
      ) ;any
    ) ;define

    (define (%dedupe lst)
      (let loop
        ((rest lst) (seen '()) (acc '()))
        (if (null? rest)
          (reverse acc)
          (if (member (car rest) seen)
            (loop (cdr rest) seen acc)
            (loop (cdr rest) (cons (car rest) seen) (cons (car rest) acc))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (pi-compass-detect-topics cwd codemap)
      (let* ((top-dirs (%top-dir-names codemap))
             (topics
               (filter
                 (lambda (n) (not (member n '("node_modules" "dist"))))
                 top-dirs
               ) ;filter
             ) ;topics
             (all-names (map string-downcase top-dirs))
             (key-paths
               (map (lambda (kf) (string-downcase (pi-compass-key-file-path kf)))
                 (pi-compass-codemap-key-files codemap)
               ) ;map
             ) ;key-paths
             (with-testing (if (%contains-any? all-names PI_COMPASS_TEST_INDICATORS)
                             (append topics (list "testing"))
                             topics
                           ) ;if
             ) ;with-testing
             (with-ci (if (or (%contains-any? all-names PI_COMPASS_CI_INDICATORS)
                            (%key-paths-mention-ci? key-paths)
                          ) ;or
                        (append with-testing (list "ci"))
                        with-testing
                      ) ;if
             ) ;with-ci
             (with-db (if (%contains-any? all-names PI_COMPASS_DB_INDICATORS)
                        (append with-ci (list "database"))
                        with-ci
                      ) ;if
             ) ;with-db
            ) ;
        (%dedupe with-db)
      ) ;let*
    ) ;define

    (define (%tree-dirs entries prefix depth acc)
      (if (null? entries)
        acc
        (let ((entry (car entries)))
          (if (pi-compass-entry-dir? entry)
            (let* ((name (pi-compass-entry-name entry))
                   (rel (if (string-null? prefix) name (string-append prefix "/" name)))
                   (acc1 (append acc (list (cons rel name))))
                   (acc2 (if (< depth 2)
                           (%tree-dirs (pi-compass-entry-children entry) rel (+ depth 1) acc1)
                           acc1
                         ) ;if
                   ) ;acc2
                  ) ;
              (%tree-dirs (cdr entries) prefix depth acc2)
            ) ;let*
            (%tree-dirs (cdr entries) prefix depth acc)
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%find-topic-dir cwd topic codemap)
      (let* ((dirs (%tree-dirs (pi-compass-codemap-tree codemap) "" 1 '()))
             (hit
               (find (lambda (pair) (string=? (cdr pair) topic)) dirs)
             ) ;hit
            ) ;
        (cond (hit (car hit))
              ((path-dir? (path (path->string (path-join (path cwd) topic)))) topic)
              ((path-dir?
                 (path (path->string (path-join (path cwd) "src" topic)))
               ) ;path-dir?
               (string-append "src/" topic)
              ) ;
              (else #f)
        ) ;cond
      ) ;let*
    ) ;define

    (define (%topic-steps cwd topic codemap)
      (let ((topic-dir (%find-topic-dir cwd topic codemap)))
        (if topic-dir
          (pi-compass-collect-tour-files (path->string (path-join (path cwd) topic-dir))
            cwd
          ) ;pi-compass-collect-tour-files
          '()
        ) ;if
      ) ;let
    ) ;define

    (define (%extra-steps cwd topic)
      (cond ((string=? topic "testing") (pi-compass-tour-test-steps cwd))
            ((string=? topic "ci") (pi-compass-tour-ci-steps cwd))
            (else '())
      ) ;cond
    ) ;define

    (define (pi-compass-build-tour cwd topic codemap)
      (pi-compass-tour (pi-compass-codemap-project-id codemap)
        topic
        (pi-iso-8601-utc-now)
        (append (%topic-steps cwd topic codemap) (%extra-steps cwd topic))
      ) ;pi-compass-tour
    ) ;define

    (define (%render-steps steps topic)
      (let loop
        ((rest steps) (i 1) (acc (list (string-append "## Code Tour: " topic) "")))
        (if (null? rest)
          (string-join acc "\n")
          (let ((step (car rest)))
            (loop (cdr rest)
              (+ i 1)
              (append acc
                (list (string-append "**" (number->string i) ".** `" (pi-compass-step-file step) "`")
                  (string-append "   " (pi-compass-step-description step))
                  ""
                ) ;list
              ) ;append
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (pi-compass-render-tour tour)
      (let ((steps (pi-compass-tour-steps tour)) (topic (pi-compass-tour-topic tour)))
        (if (null? steps)
          (string-append "No files found for topic \"" topic "\".")
          (%render-steps steps topic)
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
