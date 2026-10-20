;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension discovery)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii path)
    (liii os)
    (liii sort)
    (liii pi config)
    (liii pi read)
  ) ;import
  (export pi-scan-extension-dir
    pi-discover-extensions
    pi-default-builtin-extensions-dir
  ) ;export
  (begin

    (define (pi-default-builtin-extensions-dir)
      (let ((env-dir (get-environment-variable "PI_BUILTIN_EXTENSIONS_DIR")))
        (cond
         ((and (string? env-dir) (not (string-null? (string-trim-both env-dir))))
          env-dir
         ) ;
         (else
           (let ((candidate (path "tools/pi/liii/pi/extensions")))
             (if (and (path-exists? candidate) (path-dir? candidate))
               (path->string (path-resolve candidate))
               "tools/pi/liii/pi/extensions"
             ) ;if
           ) ;let
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (%entry-candidate dir-str name)
      (if (or (string-null? name) (char=? (string-ref name 0) #\.))
        #f
        (let* ((sub-path (path-join (path dir-str) name))
               (full-sub-str (path->string (path-resolve sub-path)))
              ) ;
          (cond
           ((and (path-file? sub-path) (string=? (path-suffix sub-path) ".scm"))
            full-sub-str
           ) ;
           ((path-dir? sub-path)
            (let ((index-path (path-join sub-path "index.scm")))
              (if (and (path-exists? index-path) (path-file? index-path))
                (path->string (path-resolve index-path))
                #f
              ) ;if
            ) ;let
           ) ;
           (else #f)
          ) ;cond
        ) ;let*
      ) ;if
    ) ;define

    (define (pi-scan-extension-dir dir)
      (let ((p (if (path? dir) dir (path dir))))
        (if (not (and (path-exists? p) (path-dir? p)))
          '()
          (let* ((dir-str (path->string (path-resolve p)))
                 (entries (catch #t (lambda () (listdir dir-str)) (lambda args '())))
                 (results '())
                ) ;
            (for-each
              (lambda (name)
                (let ((cand (%entry-candidate dir-str name)))
                  (if cand (set! results (cons cand results)))
                ) ;let
              ) ;lambda
              entries
            ) ;for-each
            (list-sort string<? results)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%opt-key-value args key default-val)
      (let loop
        ((rest args))
        (cond
         ((or (null? rest) (null? (cdr rest))) default-val)
         ((eq? (car rest) key) (cadr rest))
         (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%dir-entries-if-valid dir-str)
      (if (and (string? dir-str) (path-exists? (path dir-str)) (path-dir? (path dir-str)))
        (pi-scan-extension-dir dir-str)
        '()
      ) ;if
    ) ;define

    (define (%resolve-explicit-items explicit cwd-dir)
      (let loop
        ((items (if (list? explicit) explicit '())) (acc '()))
        (if (null? items)
          (reverse acc)
          (let ((item (car items)))
            (if
              (and (string? item) (not (string-null? (string-trim-both item))))
              (loop (cdr items) (cons (pi-resolve-path item cwd-dir) acc))
              (loop (cdr items) acc)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%dedup-paths-keep-order paths)
      (let ((deduped '()) (seen '()))
        (for-each
          (lambda (cand)
            (let ((norm-path (path->string (path-resolve (path cand)))))
              (if (not (member norm-path seen))
                (begin
                  (set! seen (cons norm-path seen))
                  (set! deduped (cons norm-path deduped))
                ) ;begin
              ) ;if
            ) ;let
          ) ;lambda
          paths
        ) ;for-each
        (reverse deduped)
      ) ;let
    ) ;define

    (define (pi-discover-extensions . options)
      (let* ((builtin-dir (%opt-key-value options :builtin-dir (pi-default-builtin-extensions-dir))
             ) ;builtin-dir
             (agent-dir (%opt-key-value options :agent-dir (pi-get-agent-dir)))
             (cwd-dir (%opt-key-value options :cwd (getcwd)))
             (explicit (%opt-key-value options :explicit-extensions '()))
             (global-dir-str
               (if (string? agent-dir)
                 (path->string (path-join (path agent-dir) "extensions"))
                 #f
               ) ;if
             ) ;global-dir-str
             (project-dir-str
               (if (string? cwd-dir)
                 (path->string (path-join (path cwd-dir) ".pi" "extensions"))
                 #f
               ) ;if
             ) ;project-dir-str
             (builtin-list (%dir-entries-if-valid builtin-dir))
             (global-list (%dir-entries-if-valid global-dir-str))
             (project-list (%dir-entries-if-valid project-dir-str))
             (explicit-list (%resolve-explicit-items explicit cwd-dir))
             (all-candidates (append builtin-list global-list project-list explicit-list))
            ) ;
        (%dedup-paths-keep-order all-candidates)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
