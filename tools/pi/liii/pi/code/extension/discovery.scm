;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code extension discovery)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii list)
    (liii path)
    (liii sys)
    (liii os)
    (liii sort)
    (liii pi code tools read)
  ) ;import
  (export pi-scan-extension-dir
    pi-discover-extensions
    pi-default-builtin-extensions-dir
  ) ;export
  (begin

    (define (pi-default-builtin-extensions-dir)
      (let ((env-dir (get-environment-variable "PI_BUILTIN_EXTENSIONS_DIR")))
        (if
          (and (string? env-dir) (not (string-null? (string-trim-both env-dir))))
          env-dir
          (let* ((repo-root
                   (catch #t (lambda () (path-parent (path-parent (executable)))) (lambda _ #f))
                 ) ;repo-root
                 (candidates
                   (append (list (path "tools/pi/extensions")
                             (path "extensions")
                             (path "../tools/pi/extensions")
                             (path "../../tools/pi/extensions")
                           ) ;list
                     (if repo-root
                       (list (path-join repo-root "tools/pi/extensions")
                         (path-join repo-root "share/goldfish/tools/pi/extensions")
                       ) ;list
                       '()
                     ) ;if
                   ) ;append
                 ) ;candidates
                 (found
                   (find (lambda (c) (and (path-exists? c) (path-dir? c))) candidates)
                 ) ;found
                ) ;
            (if found (path->string (path-resolve found)) "tools/pi/extensions")
          ) ;let*
        ) ;if
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
      (let* ((builtin-dir (if (memq :builtin-dir options)
                            (pi-keyword-value :builtin-dir options #f)
                            (pi-default-builtin-extensions-dir)
                          ) ;if
             ) ;builtin-dir
             (agent-dir (pi-keyword-value :agent-dir options (pi-get-agent-dir)))
             (cwd-dir (pi-keyword-value :cwd options (getcwd)))
             (explicit (pi-keyword-value :explicit-extensions options '()))
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
