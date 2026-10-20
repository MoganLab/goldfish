;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compass packages)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii pi compass types)
    (liii pi compass packages read)
    (liii pi compass packages toml)
  ) ;import
  (export pi-compass-read-text pi-compass-read-json pi-compass-object-keys
    pi-compass-detect-packages
  ) ;export
  (begin

    (define (%exists? p)
      (path-exists? (path p))
    ) ;define

    (define (%npm-package cwd)
      (let ((pkg
              (pi-compass-read-json (path->string (path-join (path cwd) "package.json")))
            ) ;pkg
           ) ;
        (if (not pkg)
          #f
          (let* ((deps (pi-compass-object-keys (pi-compass-jref pkg "dependencies" '())))
                 (dev-deps (pi-compass-object-keys (pi-compass-jref pkg "devDependencies" '())))
                 (name (pi-compass-jref pkg "name" ""))
                 (version (pi-compass-jref pkg "version" #f))
                ) ;
            (pi-compass-package "npm"
              (if (string? name) name "")
              (if (string? version) version #f)
              (append deps dev-deps)
            ) ;pi-compass-package
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%go-dep-name line)
      (if (not (string-starts? line "\t"))
        #f
        (let ((tokens (string-tokenize (string-trim-both line))))
          (if (pair? tokens) (car tokens) #f)
        ) ;let
      ) ;if
    ) ;define

    (define (%go-module-name lines)
      (let ((line
              (find (lambda (l) (string-starts? (string-trim-both l) "module ")) lines)
            ) ;line
           ) ;
        (if line
          (let ((trimmed (string-trim-both line)))
            (string-trim-both (substring trimmed 7 (string-length trimmed)))
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (%go-package cwd)
      (let ((text (pi-compass-read-text (path->string (path-join (path cwd) "go.mod")))))
        (if (not text)
          #f
          (let* ((lines (string-split text "\n"))
                 (deps (filter (lambda (x) x) (map %go-dep-name lines)))
                ) ;
            (pi-compass-package "go" (or (%go-module-name lines) "") #f deps)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%composer-package cwd)
      (let ((pkg
              (pi-compass-read-json (path->string (path-join (path cwd) "composer.json")))
            ) ;pkg
           ) ;
        (if (not pkg)
          #f
          (let ((name (pi-compass-jref pkg "name" "")))
            (pi-compass-package "composer"
              (if (string? name) name "")
              #f
              (pi-compass-object-keys (pi-compass-jref pkg "require" '()))
            ) ;pi-compass-package
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%one-package result)
      (if result (list result) '())
    ) ;define

    (define (%gradle-package cwd)
      (if
        (or
          (%exists? (path->string (path-join (path cwd) "build.gradle")))
          (%exists? (path->string (path-join (path cwd) "build.gradle.kts")))
        ) ;or
        (list (pi-compass-package "gradle" "" #f '()))
        '()
      ) ;if
    ) ;define

    (define (%maven-package cwd)
      (if
        (%exists? (path->string (path-join (path cwd) "pom.xml")))
        (list (pi-compass-package "maven" "" #f '()))
        '()
      ) ;if
    ) ;define

    (define (pi-compass-detect-packages cwd)
      (append (%one-package (%npm-package cwd))
        (%one-package (%go-package cwd))
        (%one-package (pi-compass-detect-cargo-package cwd))
        (%one-package (pi-compass-detect-pyproject-package cwd))
        (%one-package (%composer-package cwd))
        (%gradle-package cwd)
        (%maven-package cwd)
      ) ;append
    ) ;define

  ) ;begin
) ;define-library
