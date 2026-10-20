;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compass tree)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii sort)
    (liii path)
    (liii pi compass types)
  ) ;import
  (export PI_COMPASS_IGNORED_DIRS
    pi-compass-build-directory-tree
    pi-compass-format-directory-tree
  ) ;export
  (begin

    (define PI_COMPASS_IGNORED_DIRS
      '("node_modules" ".git" "dist" "build" ".next" ".nuxt" "__pycache__"
        ".tox" ".mypy_cache" ".pytest_cache" "target" ".gradle" ".idea"
        ".vscode" "vendor" "coverage" ".turbo" ".cache")
    ) ;define

    (define (%visible? name)
      (and (not (member name PI_COMPASS_IGNORED_DIRS))
        (or (not (string-starts? name ".")) (string=? name ".github"))
      ) ;and
    ) ;define

    (define (%sorted-names dir)
      (catch #t
        (lambda () (list-sort string<? (vector->list (path-list (path dir)))))
        (lambda _ '())
      ) ;catch
    ) ;define

    (define (%entry-for full name remaining)
      (cond
       ((path-dir? (path full))
        (pi-compass-directory-entry name
          "dir"
          (if (> remaining 1) (%scan full (- remaining 1)) #f)
        ) ;pi-compass-directory-entry
       ) ;
       ((path-file? (path full)) (pi-compass-directory-entry name "file" #f))
       (else #f)
      ) ;cond
    ) ;define

    (define (%scan dir remaining)
      (if (<= remaining 0)
        '()
        (let loop
          ((names (%sorted-names dir)) (acc '()))
          (if (null? names)
            (reverse acc)
            (let ((name (car names)))
              (if (not (%visible? name))
                (loop (cdr names) acc)
                (let ((full (path->string (path-join (path dir) name))))
                  (let ((entry (%entry-for full name remaining)))
                    (loop (cdr names) (if entry (cons entry acc) acc))
                  ) ;let
                ) ;let
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (pi-compass-build-directory-tree cwd . maybe-depth)
      (let ((depth (if (pair? maybe-depth) (car maybe-depth) 2)))
        (%scan cwd depth)
      ) ;let
    ) ;define

    (define (%format-entries entries indent)
      (let loop
        ((rest entries) (acc '()))
        (if (null? rest)
          (reverse acc)
          (let* ((entry (car rest))
                 (last? (null? (cdr rest)))
                 (prefix (if last? "└── " "├── "))
                 (name (pi-compass-entry-name entry))
                 (suffix (if (pi-compass-entry-dir? entry) "/" ""))
                 (line (string-append indent prefix name suffix))
                 (child-indent (string-append indent (if last? "    " "│   ")))
                 (child-lines (%format-entries (pi-compass-entry-children entry) child-indent))
                ) ;
            (loop (cdr rest) (append (reverse child-lines) (cons line acc)))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (pi-compass-format-directory-tree entries . maybe-indent)
      (let ((indent (if (pair? maybe-indent) (car maybe-indent) "")))
        (string-join (%format-entries entries indent) "\n")
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
