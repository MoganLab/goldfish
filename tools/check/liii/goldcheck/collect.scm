;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii goldcheck collect)
  (import (scheme base)
    (liii base)
    (liii path)
    (liii string)
    (liii goldtool changed)
    (liii goldcheck path)
  ) ;import
  (export collect-scheme-files collect-positional-files collect-repo-files
    resolve-target-files
  ) ;export
  (begin

    (define (builtin-ignored-dir? name)
      (or (string=? name ".git")
        (string=? name "build")
        (string=? name ".pi")
        (string=? name ".cache")
        (string=? name ".xmake")
      ) ;or
    ) ;define

    (define (collect-scheme-files dir-path)
      (let ((entries (path-list-path (path dir-path))))
        (let loop
          ((i 0) (acc '()))
          (if (>= i (vector-length entries))
            acc
            (let* ((entry (vector-ref entries i))
                   (s (path->string entry))
                   (bname (path-name (path s)))
                  ) ;
              (cond
               ((path-file? entry)
                (if (string-ends? s ".scm")
                  (loop (+ i 1) (cons (relativize-path s) acc))
                  (loop (+ i 1) acc)
                ) ;if
               ) ;
               ((path-dir? entry)
                (if (builtin-ignored-dir? bname)
                  (loop (+ i 1) acc)
                  (loop (+ i 1) (append (collect-scheme-files s) acc))
                ) ;if
               ) ;
               (else (loop (+ i 1) acc))
              ) ;cond
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (collect-positional-files positionals)
      (let loop
        ((pos positionals) (acc '()))
        (if (null? pos)
          acc
          (let ((p (car pos)))
            (cond
             ((path-file? (path p))
              (if (string-ends? p ".scm")
                (loop (cdr pos) (append acc (list (relativize-path p))))
                (loop (cdr pos) acc)
              ) ;if
             ) ;
             ((path-dir? (path p))
              (let ((sub (collect-scheme-files p)))
                (loop (cdr pos) (append acc sub))
              ) ;let
             ) ;
             (else (loop (cdr pos) acc))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (collect-repo-files)
      (let* ((root (g_project-root))
             (start-dir
               (if (and root (string? root) (not (string=? root ""))) root ".")
             ) ;start-dir
            ) ;
        (collect-scheme-files start-dir)
      ) ;let*
    ) ;define

    (define (resolve-target-files parser)
      (let ((since (parser 'changed-since)) (positionals (parser :positionals)))
        (cond
          (since
            (if (null? positionals)
              (changed-scheme-files-since since)
              (apply append
                (map (lambda (p) (changed-scheme-files-since since p)) positionals)
              ) ;apply
            ) ;if
          ) ;since
          ((not (null? positionals)) (collect-positional-files positionals))
          (else (collect-repo-files))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
