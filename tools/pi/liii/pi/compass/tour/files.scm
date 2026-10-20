;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compass tour files)
  (import (scheme base)
    (liii os)
    (liii string)
    (liii list)
    (liii sort)
    (liii path)
    (liii pi compass types)
  ) ;import
  (export PI_COMPASS_TEST_INDICATORS PI_COMPASS_CI_INDICATORS
    PI_COMPASS_DB_INDICATORS PI_COMPASS_SOURCE_EXTENSIONS
    pi-compass-sorted-names pi-compass-collect-tour-files
    pi-compass-tour-test-steps pi-compass-tour-ci-steps
    pi-compass-files-to-steps pi-compass-describe-file
  ) ;export
  (begin

    (define PI_COMPASS_TEST_INDICATORS '("test" "tests" "spec" "__tests__" "e2e"))
    (define PI_COMPASS_CI_INDICATORS '(".github" ".gitlab-ci.yml" "Jenkinsfile"))
    (define PI_COMPASS_DB_INDICATORS
      '("migrations" "prisma" "drizzle" "alembic" "db" "database")
    ) ;define

    (define PI_COMPASS_SOURCE_EXTENSIONS
      '(".ts" ".tsx" ".js" ".jsx" ".py" ".go" ".rs" ".java" ".rb" ".php" ".ex"
        ".exs" ".kt" ".swift" ".c" ".cpp" ".h" ".yml" ".yaml" ".toml" ".json"
        ".scm" ".sld" ".ss")
    ) ;define

    (define (pi-compass-sorted-names dir)
      (catch #t
        (lambda () (list-sort string<? (vector->list (path-list (path dir)))))
        (lambda _ '())
      ) ;catch
    ) ;define

    (define (%norm-slash s)
      (if (os-windows?) (string-replace s "\\" "/") s)
    ) ;define

    (define (%rel-path root full)
      (let* ((n-root (%norm-slash root))
             (n-full (%norm-slash full))
             (prefix (string-append n-root "/"))
            ) ;
        (if (string-starts? n-full prefix)
          (substring n-full (string-length prefix) (string-length n-full))
          n-full
        ) ;if
      ) ;let*
    ) ;define

    (define (%source-file? name)
      (member (path-suffix (path name)) PI_COMPASS_SOURCE_EXTENSIONS)
    ) ;define

    (define (%collect-entry full name root depth)
      (cond ((string-starts? name ".") '())
            ((and (path-file? (path full)) (%source-file? name))
             (list (%rel-path root full))
            ) ;
            ((path-dir? (path full)) (%collect-files full root (- depth 1)))
            (else '())
      ) ;cond
    ) ;define

    (define (%collect-files dir root depth)
      (if (<= depth 0)
        '()
        (apply append
          (map
            (lambda (name)
              (%collect-entry (path->string (path-join (path dir) name)) name root depth)
            ) ;lambda
            (pi-compass-sorted-names dir)
          ) ;map
        ) ;apply
      ) ;if
    ) ;define

    (define (%take n lst)
      (if (or (= n 0) (null? lst)) '() (cons (car lst) (%take (- n 1) (cdr lst))))
    ) ;define

    (define (pi-compass-files-to-steps files)
      (map (lambda (f) (pi-compass-tour-step f (pi-compass-describe-file f))) files)
    ) ;define

    (define (pi-compass-collect-tour-files dir root)
      (pi-compass-files-to-steps (%take 15 (%collect-files dir root 3)))
    ) ;define

    (define (%test-dir-files cwd dir)
      (let ((full (path->string (path-join (path cwd) dir))))
        (if (path-dir? (path full))
          (pi-compass-files-to-steps (%take 5 (%collect-files full cwd 2)))
          '()
        ) ;if
      ) ;let
    ) ;define

    (define (pi-compass-tour-test-steps cwd)
      (apply append
        (map (lambda (dir) (%test-dir-files cwd dir)) PI_COMPASS_TEST_INDICATORS)
      ) ;apply
    ) ;define

    (define (%workflow-steps cwd)
      (let ((workflow-dir (path->string (path-join (path cwd) ".github" "workflows"))))
        (if (not (path-dir? (path workflow-dir)))
          '()
          (map (lambda (name)
                 (pi-compass-tour-step (string-append ".github/workflows/" name)
                   (string-append "CI workflow: " name)
                 ) ;pi-compass-tour-step
               ) ;lambda
            (pi-compass-sorted-names workflow-dir)
          ) ;map
        ) ;if
      ) ;let
    ) ;define

    (define (%ci-other-steps cwd)
      (let loop
        ((files '(".gitlab-ci.yml" "Jenkinsfile")) (acc '()))
        (if (null? files)
          (reverse acc)
          (let ((rel (car files)))
            (loop (cdr files)
              (if
                (path-exists?
                  (path (path->string (path-join (path cwd) rel)))
                ) ;path-exists?
                (cons (pi-compass-tour-step rel (string-append "CI configuration: " rel)) acc)
                acc
              ) ;if
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (pi-compass-tour-ci-steps cwd)
      (append (%workflow-steps cwd) (%ci-other-steps cwd))
    ) ;define

    (define (%strip-test-marker base)
      (let* ((ti (string-position "test" base))
             (si (string-position "spec" base))
             (idx (cond ((and ti si) (min ti si)) (ti ti) (si si) (else #f)))
            ) ;
        (if (not idx)
          base
          (let* ((marker-len 4)
                 (start
                   (if
                     (and (> idx 0) (member (string-ref base (- idx 1)) '(#\-
                                                                          #\_
                                                                          #\.)))
                     (- idx 1)
                     idx
                   ) ;if
                 ) ;start
                 (end (+ idx marker-len))
                ) ;
            (string-append (substring base 0 start)
              (substring base end (string-length base))
            ) ;string-append
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (%test-description base-name)
      (string-append "Test file for " (%strip-test-marker base-name))
    ) ;define

    (define (%index-description parent)
      (if parent
        (string-append "Entry point for " parent " module")
        "Package entry point"
      ) ;if
    ) ;define

    (define (%keyword-description file-name base-name)
      (cond ((string-contains? file-name "config")
             (string-append "Configuration: " base-name)
            ) ;
            ((or (string-contains? file-name "route") (string-contains? file-name "router"))
             (string-append "Routing: " base-name)
            ) ;
            ((string-contains? file-name "middleware")
             (string-append "Middleware: " base-name)
            ) ;
            ((or (string-contains? file-name "model") (string-contains? file-name "schema"))
             (string-append "Data model: " base-name)
            ) ;
            ((string-contains? file-name "service") (string-append "Service: " base-name))
            ((or (string-contains? file-name "controller")
               (string-contains? file-name "handler")
             ) ;or
             (string-append "Handler: " base-name)
            ) ;
            ((or (string-contains? file-name "util") (string-contains? file-name "helper"))
             (string-append "Utility: " base-name)
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (pi-compass-describe-file file-path)
      (let* ((parts (string-split file-path "/"))
             (file-name (car (reverse parts)))
             (base-name (path-stem (path file-name)))
             (parent
               (if (> (length parts) 1) (list-ref parts (- (length parts) 2)) #f)
             ) ;parent
             (keyword (%keyword-description file-name base-name))
            ) ;
        (cond ((or (string-contains? file-name "test") (string-contains? file-name "spec"))
               (%test-description base-name)
              ) ;
              ((string=? base-name "index") (%index-description parent))
              (keyword keyword)
              (else (string-append base-name " module"))
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
