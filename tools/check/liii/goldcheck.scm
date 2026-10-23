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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define (%goldcheck-common-dirname path-str)
  (let loop
    ((i (- (string-length path-str) 1)))
    (cond ((< i 0) ".")
          ((or (char=? (string-ref path-str i) #\/) (char=? (string-ref path-str i) #\\))
           (if (= i 0) "." (substring path-str 0 i))
          ) ;
          (else (loop (- i 1)))
    ) ;cond
  ) ;let
) ;define

(set! *load-path*
  (append (list "../common" "tools/common")
    (map (lambda (root) (string-append (%goldcheck-common-dirname root) "/common"))
      *load-path*
    ) ;map
    *load-path*
  ) ;append
) ;set!

(define-library (liii goldcheck)
  (import (scheme base)
    (scheme file)
    (scheme write)
    (scheme process-context)
    (liii base)
    (liii os)
    (liii path)
    (liii string)
    (liii list)
    (liii argparse)
    (liii json)
    (liii par)
    (srfi srfi-133)
    (liii goldtool-changed)
    (liii goldcheck path)
    (liii goldcheck config)
    (liii goldcheck rules)
    (liii goldcheck layer)
    (liii goldcheck only)
  ) ;import
  (export main check-file-lines check-tree-depth check-file check-files
    make-check-arg-parser collect-scheme-files format-violation
    collect-repo-files collect-positional-files display-help
  ) ;export
  (begin

    (define (make-check-arg-parser)
      (let ((parser (make-argument-parser '((command . "check")
                                            (skip-value-options "-m" "--mode"
                                              "-I" "-A")
                                            (skip-prefix-options "-m="
                                              "--mode=")
                                            (unknown-options . positional))
                    ) ;make-argument-parser
            ) ;parser
           ) ;
        (parser :add-argument '((name . "help")
                                (short . "h")
                                (action . store-true)))
        (parser :add-argument '((name . "changed-since") (type . string)))
        (parser :add-argument '((name . "only") (type . string)))
        parser
      ) ;let
    ) ;define

    (define (check-file-rule-lines file-path cfg acc)
      (if (check-rule-enabled? cfg "max-file-lines")
        (let ((lines-limit (check-rule-limit cfg "max-file-lines")))
          (call-with-values (lambda () (check-file-lines file-path lines-limit))
            (lambda (lines viol) (if viol (cons viol acc) acc))
          ) ;call-with-values
        ) ;let
        acc
      ) ;if
    ) ;define

    (define (check-file-rule-depth file-path cfg acc)
      (if (check-rule-enabled? cfg "max-tree-depth")
        (let ((depth-limit (check-rule-limit cfg "max-tree-depth")))
          (call-with-values (lambda () (check-tree-depth file-path depth-limit))
            (lambda (max-d max-l viol) (if viol (cons viol acc) acc))
          ) ;call-with-values
        ) ;let
        acc
      ) ;if
    ) ;define

    (define (check-file-rule-layer file-path cfg acc)
      (append acc (check-layer-order file-path cfg))
    ) ;define

    (define (check-file file-path cfg)
      "检查单个文件是否违反启用的度量规则。若命中 exclude 则直接跳过返回 '()。"
      (let ((excludes (if cfg (check-config-excludes cfg) '())))
        (if (file-excluded? file-path excludes)
          '()
          (let* ((acc1 (check-file-rule-lines file-path cfg '()))
                 (acc2 (check-file-rule-depth file-path cfg acc1))
                 (acc3 (check-file-rule-layer file-path cfg acc2))
                ) ;
            (reverse acc3)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (check-files file-vector cfg)
      "检查文件向量，vector-par-map 并行检查且保序，返回违规项向量。\ncfg 是 JSON alist（含点对），无法作为捕获变量运输到 worker 会话，\n故序列化为 JSON 字符串捕获，worker 内用 string->json 重建。"
      (let ((cfg-text (if cfg (json->string cfg) "")))
        (vector-concatenate
          (vector->list
            (vector-map list->vector
              (vector-par-map
                (lambda (f)
                  (check-file f (if (string=? cfg-text "") #f (string->json cfg-text)))
                ) ;lambda
                file-vector
              ) ;vector-par-map
            ) ;vector-map
          ) ;vector->list
        ) ;vector-concatenate
      ) ;let
    ) ;define

    (define (builtin-ignored-dir? name)
      (or (string=? name ".git")
        (string=? name "build")
        (string=? name ".pi")
        (string=? name ".cache")
        (string=? name ".xmake")
      ) ;or
    ) ;define

    (define (collect-scheme-files dir-path excludes)
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
                (if (and (string-ends? s ".scm") (not (file-excluded? s excludes)))
                  (loop (+ i 1) (cons (relativize-path s) acc))
                  (loop (+ i 1) acc)
                ) ;if
               ) ;
               ((path-dir? entry)
                (if (or (builtin-ignored-dir? bname) (file-excluded? s excludes))
                  (loop (+ i 1) acc)
                  (loop (+ i 1) (append (collect-scheme-files s excludes) acc))
                ) ;if
               ) ;
               (else (loop (+ i 1) acc))
              ) ;cond
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (collect-positional-files positionals cfg)
      (let ((excludes (if cfg (check-config-excludes cfg) '())))
        (let loop
          ((pos positionals) (acc '()))
          (if (null? pos)
            acc
            (let ((p (car pos)))
              (cond
               ((path-file? (path p))
                (if (and (string-ends? p ".scm") (not (file-excluded? p excludes)))
                  (loop (cdr pos) (append acc (list (relativize-path p))))
                  (loop (cdr pos) acc)
                ) ;if
               ) ;
               ((path-dir? (path p))
                (let ((sub (collect-scheme-files p excludes)))
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

    (define (collect-repo-files cfg)
      (let* ((root (g_project-root))
             (start-dir
               (if (and root (string? root) (not (string=? root ""))) root ".")
             ) ;start-dir
             (excludes (if cfg (check-config-excludes cfg) '()))
            ) ;
        (collect-scheme-files start-dir excludes)
      ) ;let*
    ) ;define

    (define (resolve-target-files parser cfg)
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
          ((not (null? positionals)) (collect-positional-files positionals cfg))
          (else (collect-repo-files cfg))
        ) ;cond
      ) ;let
    ) ;define

    (define (report-and-exit violations)
      (if (zero? (vector-length violations))
        (begin
          (display "Check: OK")
          (newline)
          (exit 0)
        ) ;begin
        (begin
          (display (string-append "Check failed: "
                     (number->string (vector-length violations))
                     " violation(s) found"
                   ) ;string-append
          ) ;display
          (newline)
          (vector-for-each (lambda (v) (display (format-violation v)) (newline))
            violations
          ) ;vector-for-each
          (exit 1)
        ) ;begin
      ) ;if
    ) ;define

    (define (main)
      (let ((parser (make-check-arg-parser)))
        (parser :parse-argv (command-line))
        (if (parser 'help)
          (begin
            (display-help)
            (exit 0)
          ) ;begin
          (let* ((cfg0 (load-check-config))
                 (only (parser 'only))
                 (cfg (if only (restrict-check-config cfg0 only) cfg0))
                 (files (resolve-target-files parser cfg))
                 (violations (check-files (list->vector files) cfg))
                ) ;
            (report-and-exit violations)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
