;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compass project)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii path)
    (liii hashlib)
    (liii subprocess)
    (liii pi time)
  ) ;import
  (export pi-compass-hash-id pi-compass-detect-project pi-compass-project-id
    pi-compass-project-name pi-compass-project-root pi-compass-project-remote
  ) ;export
  (begin

    (define %tmp-counter 0)
    (define %tmp-prefix "pi-compass-probe-")

    (define (pi-compass-hash-id input)
      (substring (sha256 input) 0 12)
    ) ;define

    (define (%next-temp-file)
      (set! %tmp-counter (+ %tmp-counter 1))
      (path->string
        (path-join (path (path-temp-dir))
          (string-append %tmp-prefix
            (number->string (pi-current-time-millis))
            "-"
            (number->string %tmp-counter)
          ) ;string-append
        ) ;path-join
      ) ;path->string
    ) ;define

    ;; 跑一条只读探测命令并返回 stdout；失败返回 #f。每次调用都带超时。
    (define (%capture cmd cwd)
      (let* ((tmp (%next-temp-file))
             (code (catch #t
                     (lambda () (run cmd :cwd cwd :stdout tmp :stderr 'discard :timeout 5))
                     (lambda _ #f)
                   ) ;catch
             ) ;code
             (text
               (if (and code (= code 0) (path-exists? (path tmp)))
                 (path-read-text (path tmp))
                 #f
               ) ;if
             ) ;text
            ) ;
        (when (path-exists? (path tmp))
          (path-unlink (path tmp) #t)
        ) ;when
        (if text (string-trim-both text) #f)
      ) ;let*
    ) ;define

    (define (pi-compass-detect-project cwd)
      (let* ((absolute (path->string (path-resolve (path cwd))))
             (root (%capture '("git" "rev-parse" "--show-toplevel") absolute))
            ) ;
        (if (and root (not (string-null? root)))
          (let* ((name (path-name (path root)))
                 (remote (or (%capture '("git" "remote" "get-url" "origin") root) ""))
                 (remote (if (string? remote) remote ""))
                 (id (pi-compass-hash-id (if (string-null? remote) root remote)))
                ) ;
            (list (cons "id" id)
              (cons "name" name)
              (cons "root" root)
              (cons "remote" remote)
            ) ;list
          ) ;let*
          (let* ((name (path-name (path absolute))) (id (pi-compass-hash-id absolute)))
            (list (cons "id" id)
              (cons "name" name)
              (cons "root" absolute)
              (cons "remote" "")
            ) ;list
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-compass-project-id project)
      (cdr (assoc "id" project))
    ) ;define

    (define (pi-compass-project-name project)
      (cdr (assoc "name" project))
    ) ;define

    (define (pi-compass-project-root project)
      (cdr (assoc "root" project))
    ) ;define

    (define (pi-compass-project-remote project)
      (cdr (assoc "remote" project))
    ) ;define

  ) ;begin
) ;define-library
