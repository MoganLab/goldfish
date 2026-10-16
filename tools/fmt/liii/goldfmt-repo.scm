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

(define-library (liii goldfmt-repo)
  (import (liii base)
    (liii os)
    (liii string)
    (liii semver)
    (liii goldfmt-lang)
    (liii goldfmt-config)
  ) ;import
  (export flush-output stats-failed check-goldfish-version
    print-offenders check-files-or-serial check-lang
    run-repo-format run-repo-check handle-repo-target
  ) ;export
  (begin

    (define (flush-output)
      (flush-output-port (current-output-port))
    ) ;define

    (define (stats-failed stats)
      (if (> (length stats) 3) (list-ref stats 3) 0)
    ) ;define

    (define (check-goldfish-version cfg)
      (let ((required-version (fmt-config-goldfish-version cfg)))
        (when required-version
          (when
            (or (not (semver-valid? required-version))
              (not (semver=? (version) required-version))
            ) ;or
            (display (string-append "error: gf_fmt.json requires Goldfish version "
                       required-version
                       ", but current version is "
                       (version)
                     ) ;string-append
            ) ;display
            (newline)
            (exit 1)
          ) ;when
        ) ;when
      ) ;let
    ) ;define

    (define (run-repo-format cfg)
      (let loop
        ((handlers (lang-list)) (repo-failed 0))
        (if (null? handlers)
          (begin
            (display "Done.")
            (newline)
            (if (> repo-failed 0) (exit 1) #t)
          ) ;begin
          (let* ((handler (car handlers))
                 (label (lang-label handler))
                 (collect (lang-ref handler 'collect))
                 (format-files (lang-ref handler 'format-files))
                 (files (collect cfg))
                 (stats (format-files files cfg))
                 (failed (stats-failed stats))
                ) ;
            (display (string-append "=== Formatting " label " files ==="))
            (newline)
            (flush-output)
            (display
              (string-append "Total "
                label
                " files formatted: "
                (number->string (car stats))
                ", Files updated: "
                (number->string (cadr stats))
                ", Files unchanged: "
                (number->string (caddr stats))
                (if (> failed 0) (string-append ", Files failed: " (number->string failed)) "")
              ) ;string-append
            ) ;display
            (newline)
            (newline)
            (loop (cdr handlers) (+ repo-failed failed))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (print-offenders label offenders)
      (let ((n (length offenders)))
        (if (= n 0)
          (begin
            (display (string-append label ": OK"))
            (newline)
          ) ;begin
          (begin
            (display (string-append label ": " (number->string n) " file(s) need formatting")
            ) ;display
            (newline)
            (for-each (lambda (f) (display (string-append "  " f)) (newline)) offenders)
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    (define (check-files-or-serial handler files cfg)
      (let ((check-files-pair (assq 'check-files handler)))
        (if check-files-pair
          ((cdr check-files-pair) files cfg)
          (let ((check-file (lang-ref handler 'check-file)))
            (let loop
              ((fs files) (bad '()))
              (if (null? fs)
                (reverse bad)
                (let ((f (car fs)))
                  (loop (cdr fs) (if (check-file f cfg) bad (cons f bad)))
                ) ;let
              ) ;if
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (check-lang handler cfg)
      (let* ((collect (lang-ref handler 'collect)) (files (collect cfg)))
        (check-files-or-serial handler files cfg)
      ) ;let*
    ) ;define

    (define (report-repo-check-results results)
      (let ((total (apply + (map (lambda (r) (length (cdr r))) results))))
        (newline)
        (for-each (lambda (r) (print-offenders (lang-label (car r)) (cdr r)))
          (reverse results)
        ) ;for-each
        (newline)
        (if (> total 0)
          (begin
            (display (string-append "FAIL: " (number->string total) " file(s) need formatting")
            ) ;display
            (newline)
            (exit 1)
          ) ;begin
          (begin
            (display "OK: all files formatted.")
            (newline)
            (exit 0)
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    (define (run-repo-check cfg)
      (if (os-windows?)
        (begin
          (display "fmt --check: skipped on Windows (requires sh).")
          (newline)
          (exit 0)
        ) ;begin
        (let loop
          ((handlers (lang-list)) (results '()))
          (if (null? handlers)
            (report-repo-check-results results)
            (let* ((handler (car handlers)) (label (lang-label handler)))
              (display (string-append "=== Checking " label " files ==="))
              (newline)
              (flush-output)
              (loop (cdr handlers) (cons (cons handler (check-lang handler cfg)) results))
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (handle-repo-target check-flag)
      (let ((cfg (catch #t (lambda () (load-fmt-config))
                   (lambda (type info)
                     (let ((e (if (null? info) type (car info))))
                       (display (string-append "error: gf_fmt.json 解析失败 - "
                                  (if (string? e) e (object->string e))
                                ) ;string-append
                       ) ;display
                       (newline)
                       (exit 1)
                     ) ;let
                   ) ;lambda
                 ) ;catch
            ) ;cfg
           ) ;
        (if (not cfg)
          (begin
            (display "error: 找不到 gf_fmt.json，请在项目根创建它（无路径参数时必需）。"
            ) ;display
            (newline)
            (exit 1)
          ) ;begin
          (begin
            (check-goldfish-version cfg)
            (if check-flag (run-repo-check cfg) (run-repo-format cfg))
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
