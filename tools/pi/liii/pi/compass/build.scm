;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compass build)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii path)
    (liii pi compass types)
    (liii pi compass packages)
  ) ;import
  (export PI_COMPASS_INTERESTING_SCRIPTS pi-compass-detect-build-scripts)
  (begin

    (define PI_COMPASS_INTERESTING_SCRIPTS
      '("build" "dev" "start" "test" "lint" "check" "typecheck" "format"
        "deploy" "clean" "serve" "preview" "generate" "migrate" "seed")
    ) ;define

    (define (%exists? rel cwd)
      (path-exists?
        (path (path->string (path-join (path cwd) rel)))
      ) ;path-exists?
    ) ;define

    (define (%npm-script-entries scripts)
      (let loop
        ((pairs scripts) (acc '()))
        (if (null? pairs)
          (reverse acc)
          (let* ((name (car (car pairs))) (command (cdr (car pairs))))
            (loop (cdr pairs)
              (if (member name PI_COMPASS_INTERESTING_SCRIPTS)
                (cons (pi-compass-build-script name
                        (if (string? command) command (object->string command))
                        "package.json"
                      ) ;pi-compass-build-script
                  acc
                ) ;cons
                acc
              ) ;if
            ) ;loop
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%npm-scripts cwd)
      (let ((pkg
              (pi-compass-read-json (path->string (path-join (path cwd) "package.json")))
            ) ;pkg
           ) ;
        (if (not pkg)
          '()
          (let ((scripts (pi-compass-jref pkg "scripts" '())))
            (if (pi-compass-object? scripts) (%npm-script-entries scripts) '())
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%target-name line)
      (let ((colon (string-index line #\:)))
        (if (not colon)
          #f
          (let ((candidate (string-trim-both (substring line 0 colon))))
            (if (or (string-null? candidate)
                  (char=? (string-ref candidate 0) #\.)
                  (char-whitespace? (string-ref candidate 0))
                ) ;or
              #f
              (let loop
                ((chars (string->list candidate)) (ok #t))
                (if (null? chars)
                  (if ok candidate #f)
                  (let ((ch (car chars)))
                    (if (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_) (char=? ch #\-))
                      (loop (cdr chars) ok)
                      (loop (cdr chars) #f)
                    ) ;if
                  ) ;let
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%makefile-scripts cwd)
      (let ((text (pi-compass-read-text (path->string (path-join (path cwd) "Makefile")))))
        (if (not text)
          '()
          (let loop
            ((lines (string-split text "\n")) (acc '()) (seen '()))
            (if (null? lines)
              (reverse acc)
              (let ((name (%target-name (car lines))))
                (if (and name (not (member name seen)))
                  (loop (cdr lines)
                    (cons (pi-compass-build-script name (string-append "make " name) "Makefile")
                      acc
                    ) ;cons
                    (cons name seen)
                  ) ;loop
                  (loop (cdr lines) acc seen)
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%ci-scripts cwd)
      (append (if (%exists? ".github/workflows" cwd)
                (list (pi-compass-build-script "ci" "GitHub Actions" ".github/workflows/"))
                '()
              ) ;if
        (if (%exists? ".gitlab-ci.yml" cwd)
          (list (pi-compass-build-script "ci" "GitLab CI" ".gitlab-ci.yml"))
          '()
        ) ;if
        (if (%exists? "Jenkinsfile" cwd)
          (list (pi-compass-build-script "ci" "Jenkins" "Jenkinsfile"))
          '()
        ) ;if
      ) ;append
    ) ;define

    (define (%container-scripts cwd)
      (append (if (%exists? "Dockerfile" cwd)
                (list (pi-compass-build-script "docker" "docker build" "Dockerfile"))
                '()
              ) ;if
        (if (or (%exists? "docker-compose.yml" cwd) (%exists? "docker-compose.yaml" cwd))
          (list (pi-compass-build-script "docker-compose"
                  "docker compose up"
                  "docker-compose.yml"
                ) ;pi-compass-build-script
          ) ;list
          '()
        ) ;if
      ) ;append
    ) ;define

    (define (%xmake-scripts cwd)
      (if (%exists? "xmake.lua" cwd)
        (list (pi-compass-build-script "build" "xmake build" "xmake.lua"))
        '()
      ) ;if
    ) ;define

    (define (pi-compass-detect-build-scripts cwd)
      (append (%npm-scripts cwd)
        (%makefile-scripts cwd)
        (%ci-scripts cwd)
        (%container-scripts cwd)
        (%xmake-scripts cwd)
      ) ;append
    ) ;define

  ) ;begin
) ;define-library
