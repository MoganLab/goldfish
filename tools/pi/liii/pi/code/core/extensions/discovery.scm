;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core extensions discovery)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii list)
    (liii path)
    (liii sys)
    (liii os)
    (liii sort)
    (liii pi code core tools read)
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
                   ;; 内置扩展必须先匹配可执行文件对应的安装/源码目录，
                   ;; 避免 cwd 下的 tools/pi/extensions 混入另一套 Pi 库。
                   (append (if repo-root
                             (list (path-join repo-root "tools/pi/extensions")
                               (path-join repo-root "share/goldfish/tools/pi/extensions")
                             ) ;list
                             '()
                           ) ;if
                     (list (path "tools/pi/extensions")
                       (path "extensions")
                       (path "../tools/pi/extensions")
                       (path "../../tools/pi/extensions")
                     ) ;list
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
      (map (lambda (item) (pi-resolve-path item cwd-dir))
        (filter
          (lambda (item)
            (and (string? item) (not (string-null? (string-trim-both item))))
          ) ;lambda
          (if (list? explicit) explicit '())
        ) ;filter
      ) ;map
    ) ;define

    (define (%extension-name path-str)
      (let* ((p (path path-str))
             (filename (path-name p))
             (name (if (string=? filename "index.scm") (path-name (path-parent p)) filename))
            ) ;
        (if (string-ends? name ".scm")
          (substring name 0 (- (string-length name) 4))
          name
        ) ;if
      ) ;let*
    ) ;define

    (define (%normalize-extension-paths paths)
      (map
        (lambda (p) (path->string (path-resolve (path p))))
        paths
      ) ;map
    ) ;define

    (define (%extension-entries paths)
      (map (lambda (p) (cons (%extension-name p) p))
        (%normalize-extension-paths paths)
      ) ;map
    ) ;define

    (define (%upsert-extension entries name path-str)
      (let ((replaced #f))
        (let ((updated
                (map
                  (lambda (entry)
                    (if (string=? (car entry) name)
                      (begin
                        (set! replaced #t)
                        (cons name path-str)
                      ) ;begin
                      entry
                    ) ;if
                  ) ;lambda
                  entries
                ) ;map
              ) ;updated
             ) ;
          (if replaced updated (append updated (list (cons name path-str))))
        ) ;let
      ) ;let
    ) ;define

    (define (%merge-extension-layer entries paths protected-names)
      (for-each
        (lambda (entry)
          (unless (member (car entry) protected-names)
            (set! entries (%upsert-extension entries (car entry) (cdr entry)))
          ) ;unless
        ) ;lambda
        (%extension-entries paths)
      ) ;for-each
      entries
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
             (include-builtin? (pi-keyword-value :include-builtin? options #t))
             (global-dir-str (if (or (string? agent-dir) (path? agent-dir))
                               (path->string (path-join agent-dir "extensions"))
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
             (builtin-names (map car (%extension-entries builtin-list)))
             ;; 用户级可覆盖内置；项目级可覆盖普通用户级，但不能覆盖内置名称。
             ;; include-builtin? 为 #f 时仍用内置名称仲裁，仅不重复返回内置文件。
             (global-entries (%merge-extension-layer (if include-builtin? (%extension-entries builtin-list) '())
                               global-list
                               '()
                             ) ;%merge-extension-layer
             ) ;global-entries
             (project-entries (%merge-extension-layer global-entries project-list builtin-names)
             ) ;project-entries
             (all-entries (%merge-extension-layer project-entries explicit-list '()))
            ) ;
        (map cdr all-entries)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
