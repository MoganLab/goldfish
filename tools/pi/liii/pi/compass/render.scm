;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compass render)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii pi compass types)
    (liii pi compass tree)
  ) ;import
  (export pi-compass-render-codemap pi-compass-truncate-codemap)
  (begin

    (define (%deps-lines deps)
      (if (null? deps)
        '()
        (let* ((n (length deps))
               (shown (if (> n 20) (list-take 20 deps) deps))
               (more
                 (if (> n 20) (string-append " (+" (number->string (- n 20)) " more)") "")
               ) ;more
              ) ;
          (list (string-append "  Dependencies: " (string-join shown ", ") more))
        ) ;let*
      ) ;if
    ) ;define

    (define (%package-head pkg)
      (let* ((version (pi-compass-package-version pkg))
             (version-str (if version (string-append " v" version) ""))
             (name (pi-compass-package-name pkg))
             (name-str (if (string-null? name) "(unnamed)" name))
             (manager (pi-compass-package-manager pkg))
            ) ;
        (string-append "- **" name-str "**" version-str " (" manager ")")
      ) ;let*
    ) ;define

    (define (%render-packages packages)
      (let loop
        ((rest packages) (acc '()))
        (if (null? rest)
          (reverse acc)
          (let* ((pkg (car rest))
                 (lines (cons (%package-head pkg) (%deps-lines (pi-compass-package-dependencies pkg)))
                 ) ;lines
                ) ;
            (loop (cdr rest) (append (reverse lines) acc))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%render-frameworks frameworks)
      (map
        (lambda (fw)
          (let ((version (pi-compass-jref fw "version" #f)))
            (string-append "- "
              (pi-compass-framework-name fw)
              (if (string? version) (string-append " v" version) "")
              (if (string=? (pi-compass-framework-confidence fw) "likely") " (likely)" "")
            ) ;string-append
          ) ;let
        ) ;lambda
        frameworks
      ) ;map
    ) ;define

    (define (%render-entry-points entry-points)
      (map (lambda (ep)
             (string-append "- `"
               (pi-compass-entry-point-path ep)
               "` ("
               (pi-compass-entry-point-kind ep)
               ")"
             ) ;string-append
           ) ;lambda
        entry-points
      ) ;map
    ) ;define

    (define (%render-build-scripts scripts)
      (map (lambda (s)
             (string-append "- **"
               (pi-compass-build-script-name s)
               "**: `"
               (pi-compass-build-script-command s)
               "` ("
               (pi-compass-build-script-source s)
               ")"
             ) ;string-append
           ) ;lambda
        scripts
      ) ;map
    ) ;define

    (define (%render-conventions conventions)
      (let loop
        ((rest conventions) (acc '()))
        (if (null? rest)
          acc
          (let ((c (car rest)))
            (loop (cdr rest)
              (append acc
                (list ""
                  (string-append "#### " (pi-compass-convention-source c))
                  (pi-compass-convention-content c)
                ) ;list
              ) ;append
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%render-key-files key-files)
      (map (lambda (kf)
             (string-append "- `"
               (pi-compass-key-file-path kf)
               "` -- "
               (pi-compass-key-file-description kf)
             ) ;string-append
           ) ;lambda
        key-files
      ) ;map
    ) ;define

    (define (%section title lines)
      (if (null? lines) '() (append (list "" (string-append "### " title)) lines))
    ) ;define

    (define (pi-compass-render-codemap codemap)
      (let* ((tree (pi-compass-codemap-tree codemap))
             (packages (pi-compass-codemap-packages codemap))
             (frameworks (pi-compass-codemap-frameworks codemap))
             (entry-points (pi-compass-codemap-entry-points codemap))
             (build-scripts (pi-compass-codemap-build-scripts codemap))
             (conventions (pi-compass-codemap-conventions codemap))
             (key-files (pi-compass-codemap-key-files codemap))
             (head (list (string-append "## Codebase Map: " (pi-compass-codemap-project-name codemap))
                     (string-append "Generated: "
                       (pi-compass-codemap-generated-at codemap)
                       " | Hash: "
                       (pi-compass-codemap-content-hash codemap)
                     ) ;string-append
                   ) ;list
             ) ;head
             (dir-sec
               (if (null? tree)
                 '()
                 (append (list ""
                           "### Directory Structure"
                           "```"
                           (pi-compass-format-directory-tree tree)
                           "```"
                         ) ;list
                 ) ;append
               ) ;if
             ) ;dir-sec
             (sections (append head
                         dir-sec
                         (%section "Packages" (%render-packages packages))
                         (%section "Frameworks" (%render-frameworks frameworks))
                         (%section "Entry Points" (%render-entry-points entry-points))
                         (%section "Build / Test / Deploy" (%render-build-scripts build-scripts))
                         (%section "Conventions" (%render-conventions conventions))
                         (%section "Key Files" (%render-key-files key-files))
                       ) ;append
             ) ;sections
            ) ;
        (string-join sections "\n")
      ) ;let*
    ) ;define

    (define (pi-compass-truncate-codemap markdown max-chars)
      (if (<= (string-length markdown) max-chars)
        markdown
        (let loop
          ((lines (string-split markdown "\n")) (length 0) (kept '()))
          (if (null? lines)
            (string-join (reverse kept) "\n")
            (let ((line (car lines)))
              (if (> (+ length (string-length line) 1) max-chars)
                (string-join (append (reverse kept)
                               (list "" "... (codemap truncated, run --codemap to see full output)")
                             ) ;append
                  "\n"
                ) ;string-join
                (loop (cdr lines) (+ length (string-length line) 1) (cons line kept))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
