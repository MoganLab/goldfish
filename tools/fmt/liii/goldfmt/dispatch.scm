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

(define-library (liii goldfmt dispatch)
  (import (liii base)
    (liii sys)
    (liii path)
    (liii string)
    (liii goldfmt lang)
    (liii goldfmt path)
    (liii goldfmt config)
    (liii goldfmt repo)
  ) ;import
  (export ensure-scheme-handler! scheme-handler-of dispatch-format-file
    directory-handler-for dispatch-format-directory dispatch-check-directory
    dispatch-check-file scheme-config-excludes
  ) ;export
  (begin

    (define (ensure-scheme-handler!)
      (unless (lang-for-extension ".scm")
        (eval '(import (liii goldfmt scheme)) (rootlet))
      ) ;unless
    ) ;define

    (define (scheme-handler-of)
      (ensure-scheme-handler!)
      (lang-for-extension ".scm")
    ) ;define

    (define (dispatch-format-file path-str dry-run excludes)
      (let* ((cfg (catch #t (lambda () (load-fmt-config)) (lambda (type info) #f)))
             (dummy (check-goldfish-version cfg))
             (ext (path-suffix (path path-str)))
             (handler (or (lang-for-extension ext) (scheme-handler-of)))
             (format-file (lang-ref handler 'format-file))
            ) ;
        (format-file path-str dry-run excludes)
      ) ;let*
    ) ;define

    (define (directory-handler-for extensions)
      (let ((matched (lang-for-extensions extensions)))
        (cond ((null? matched) (scheme-handler-of))
              ((null? (cdr matched)) (car matched))
              (else (display "错误: 目录格式化不支持混合语言后缀，请用单一语言的 -e"
                    ) ;display
                (newline)
                (exit 1)
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (dispatch-format-directory dir extensions excludes dry-run)
      (let* ((cfg (catch #t (lambda () (load-fmt-config)) (lambda (type info) #f)))
             (dummy (check-goldfish-version cfg))
             (handler (directory-handler-for extensions))
             (format-directory (lang-ref handler 'format-directory))
             (stats (format-directory dir extensions excludes dry-run cfg))
             (failed (stats-failed stats))
            ) ;
        (display
          (string-append "Total files formatted: "
            (number->string (car stats))
            ", Files updated: "
            (number->string (cadr stats))
            ", Files unchanged: "
            (number->string (caddr stats))
            (if (> failed 0) (string-append ", Files failed: " (number->string failed)) "")
          ) ;string-append
        ) ;display
        (newline)
        (flush-output)
        (if (> failed 0) (exit 1) #t)
      ) ;let*
    ) ;define

    (define (dispatch-check-directory dir extensions excludes)
      (let* ((cfg (catch #t (lambda () (load-fmt-config)) (lambda (type info) #f)))
             (dummy (check-goldfish-version cfg))
             (handler (directory-handler-for extensions))
             (label (lang-label handler))
             (lang-name-sym (lang-name handler))
             (cfg-excludes (if cfg (lang-excludes lang-name-sym cfg) '()))
             (all-excludes (append excludes cfg-excludes))
             (files (collect-files dir extensions all-excludes))
            ) ;
        (display (string-append "=== Checking " label " files in " dir " ==="))
        (newline)
        (flush-output)
        (let* ((offenders (check-files-or-serial handler files cfg))
               (total (length offenders))
              ) ;
          (newline)
          (print-offenders label offenders)
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
        ) ;let*
      ) ;let*
    ) ;define

    (define (dispatch-check-file path-str excludes)
      (let* ((cfg (catch #t (lambda () (load-fmt-config)) (lambda (type info) #f)))
             (dummy (check-goldfish-version cfg))
             (ext (path-suffix (path path-str)))
             (handler (or (lang-for-extension ext) (scheme-handler-of)))
             (lang-name-sym (lang-name handler))
             (cfg-excludes (if cfg (lang-excludes lang-name-sym cfg) '()))
             (all-excludes (append excludes cfg-excludes))
             (check-file (lang-ref handler 'check-file))
            ) ;
        (if (file-excluded? path-str all-excludes)
          (begin
            (display (string-append "Skipped (excluded): " path-str))
            (newline)
            (exit 0)
          ) ;begin
          (if (check-file path-str cfg)
            (begin
              (display (string-append path-str ": OK"))
              (newline)
              (exit 0)
            ) ;begin
            (begin
              (display (string-append path-str ": needs formatting"))
              (newline)
              (exit 1)
            ) ;begin
          ) ;if
        ) ;if
      ) ;let*
    ) ;define

    (define (scheme-config-excludes)
      (let ((cfg (catch #t (lambda () (load-fmt-config)) (lambda (type info) #f))))
        (if cfg (lang-excludes 'scheme cfg) '())
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
