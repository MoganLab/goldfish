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
    (liii list)
    (liii path)
    (liii string)
    (liii goldtool changed)
    (liii gfproject config)
    (liii goldcheck path)
    (liii goldcheck config)
  ) ;import
  (export collect-scheme-files collect-files-by-suffixes file-suffix-in?
    collect-positional-files collect-repo-files resolve-target-files
    resolve-no-crlf-files
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

    (define (file-suffix-in? s suffixes)
      "判断路径 s 的文件名是否以 suffixes 中任一后缀（不带点）结尾。"
      (let loop
        ((sx suffixes))
        (if (null? sx)
          #f
          (if (string-ends? s (string-append "." (car sx))) #t (loop (cdr sx)))
        ) ;if
      ) ;let
    ) ;define

    (define (collect-files-by-suffixes dir-path suffixes)
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
                (if (file-suffix-in? s suffixes)
                  (loop (+ i 1) (cons (relativize-path s) acc))
                  (loop (+ i 1) acc)
                ) ;if
               ) ;
               ((path-dir? entry)
                (if (builtin-ignored-dir? bname)
                  (loop (+ i 1) acc)
                  (loop (+ i 1) (append (collect-files-by-suffixes s suffixes) acc))
                ) ;if
               ) ;
               (else (loop (+ i 1) acc))
              ) ;cond
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (collect-scheme-files dir-path)
      (collect-files-by-suffixes dir-path '("scm"))
    ) ;define

    (define (collect-positional-files positionals . maybe-suffixes)
      (let ((suffixes (if (null? maybe-suffixes) '("scm") (car maybe-suffixes))))
        (let loop
          ((pos positionals) (acc '()))
          (if (null? pos)
            acc
            (let ((p (car pos)))
              (cond
               ((path-file? (path p))
                (if (file-suffix-in? p suffixes)
                  (loop (cdr pos) (append acc (list (relativize-path p))))
                  (loop (cdr pos) acc)
                ) ;if
               ) ;
               ((path-dir? (path p))
                (let ((sub (collect-files-by-suffixes p suffixes)))
                  (loop (cdr pos) (append acc sub))
                ) ;let
               ) ;
               (else (loop (cdr pos) acc))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (collect-repo-files)
      (let* ((root (gfproject-root))
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

    (define (resolve-no-crlf-files parser cfg)
      "为 no-crlf 规则解析目标文件：全仓时按配置 suffix/path 限定，\n显式 PATH 与 --changed-since 时按用户指定范围并按 suffix 过滤。"
      (let* ((suffixes (check-rule-suffixes cfg "no-crlf"))
             (since (parser 'changed-since))
             (positionals (parser :positionals))
            ) ;
        (cond
          (since
            (filter (lambda (f) (file-suffix-in? f suffixes))
              (if (null? positionals)
                (changed-files-since since)
                (apply append (map (lambda (p) (changed-files-since since p)) positionals))
              ) ;if
            ) ;filter
          ) ;since
          ((not (null? positionals)) (collect-positional-files positionals suffixes))
          (else
            (let* ((paths (check-rule-paths cfg "no-crlf"))
                   (root (gfproject-root))
                   (start-dir
                     (if (and root (string? root) (not (string=? root ""))) root ".")
                   ) ;start-dir
                  ) ;
              (if (null? paths)
                (collect-files-by-suffixes start-dir suffixes)
                (apply append (map (lambda (p) (collect-files-by-suffixes p suffixes)) paths))
              ) ;if
            ) ;let*
          ) ;else
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
