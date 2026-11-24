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
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; Markdown 语言处理器：通过 cmark domain 的
;; Markdown -> md-stem -> Markdown 往返转换统一排版，包括表格列宽对齐。
(define-library (liii goldfmt markdown)
  (import (liii base)
    (liii path)
    (liii string)
    (liii list)
    (liii cmark)
    (liii goldfmt cache)
    (liii goldfmt lang)
    (liii goldfmt config)
    (liii goldfmt parallel)
  ) ;import
  (export format-markdown-string markdown-extensions markdown-format-file-quiet
    markdown-fmt-one markdown-check-file-quiet markdown-check-one
    markdown-check-files
  ) ;export
  (begin

    (define markdown-extensions '(".md" ".markdown"))

    (define (format-markdown-string source)
      (md-stem->markdown (markdown->md-stem source))
    ) ;define

    (define (format-file-dry-run path-str)
      (let ((err #f)
            (formatted
              (catch #t
                (lambda () (format-markdown-string (path-read-text (path path-str))))
                (lambda (tag info) (set! err (cons tag info)) #f)
              ) ;catch
            ) ;formatted
           ) ;
        (if err
          (begin
            (print-fmt-failure path-str (fmt-extract-error-message (car err) (cdr err)))
            (exit 1)
          ) ;begin
          (begin
            (display formatted)
            (flush-output-port (current-output-port))
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    (define (markdown-format-file-quiet path-str)
      (if (fmt-cache-hit? path-str)
        (list 'cached #f)
        (let* ((p (path path-str))
               (original (path-read-text p))
               (err #f)
               (formatted
                 (catch #t
                   (lambda () (format-markdown-string original))
                   (lambda (tag info) (set! err (cons tag info)) #f)
                 ) ;catch
               ) ;formatted
              ) ;
          (cond
            (err (list 'failed (fmt-extract-error-message (car err) (cdr err))))
            ((string=? original formatted) (fmt-cache-touch path-str) (list 'unchanged #f))
            (else (path-write-text p formatted)
              (fmt-cache-touch path-str)
              (list 'updated #f)
            ) ;else
          ) ;cond
        ) ;let*
      ) ;if
    ) ;define

    (define (print-result file result)
      (print-fmt-result-line file (cadr result) (caddr result))
    ) ;define

    (define (markdown-fmt-one path)
      (run-one-task path
        markdown-format-file-quiet
        (lambda (tag info) (list 'failed (fmt-extract-error-message tag info)))
      ) ;run-one-task
    ) ;define

    (define (format-file-list files excludes)
      (let* ((targets
               (filter (lambda (f) (not (file-excluded? f excludes))) files)
             ) ;targets
             (results (pool-for-each markdown-fmt-one targets print-result))
            ) ;
        (values (length results)
          (count-status 'updated results)
          (count-status 'cached results)
          (count-status 'failed results)
        ) ;values
      ) ;let*
    ) ;define

    (define (format-single-file path-str dry-run excludes)
      (if (file-excluded? path-str excludes)
        (begin
          (display (string-append "Skipped (excluded): " path-str))
          (newline)
          #t
        ) ;begin
        (if dry-run
          (format-file-dry-run path-str)
          (let* ((result (markdown-format-file-quiet path-str))
                 (status (car result))
                 (msg (cadr result))
                ) ;
            (cond ((eq? status 'cached) #f)
                  ((eq? status 'failed) (print-fmt-failure path-str msg) (exit 1))
                  (else (print-fmt-result-line path-str status msg))
            ) ;cond
            (display (string-append "Total files formatted: 1, Files updated: "
                       (if (eq? status 'updated) "1" "0")
                       ", Files cached: "
                       (if (eq? status 'cached) "1" "0")
                     ) ;string-append
            ) ;display
            (newline)
            (flush-output-port (current-output-port))
            #t
          ) ;let*
        ) ;if
      ) ;if
    ) ;define

    (define (format-directory dir extensions excludes dry-run . maybe-cfg)
      (if dry-run
        (begin
          (display "错误: --dry-run 选项仅支持单个文件")
          (newline)
          (exit 1)
        ) ;begin
        (let* ((cfg (if (null? maybe-cfg) #f (car maybe-cfg)))
               (all-excludes (append excludes (if cfg (lang-excludes 'markdown cfg) '())))
              ) ;
          (call-with-values (lambda ()
                              (format-file-list (collect-files-dfs dir extensions all-excludes) all-excludes)
                            ) ;lambda
            (lambda (total updated cached failed) (list total updated cached failed))
          ) ;call-with-values
        ) ;let*
      ) ;if
    ) ;define

    (define (markdown-collect cfg)
      (append-map
        (lambda (p)
          (if (path-dir? (path p))
            (collect-files p (lang-suffixes 'markdown cfg) (lang-excludes 'markdown cfg))
            '()
          ) ;if
        ) ;lambda
        (lang-paths 'markdown cfg)
      ) ;append-map
    ) ;define

    (define (markdown-format-files files cfg)
      (call-with-values (lambda () (format-file-list files (lang-excludes 'markdown cfg)))
        (lambda (total updated cached failed) (list total updated cached failed))
      ) ;call-with-values
    ) ;define

    (define (markdown-check-file-quiet path-str)
      (let* ((ondisk (path-read-text (path path-str)))
             (formatted (catch #t (lambda () (format-markdown-string ondisk)) (lambda (tag info) #f))
             ) ;formatted
            ) ;
        (and formatted (string=? ondisk formatted))
      ) ;let*
    ) ;define

    (define (markdown-check-file path-str cfg)
      (if (file-excluded? path-str (lang-excludes 'markdown cfg))
        #t
        (markdown-check-file-quiet path-str)
      ) ;if
    ) ;define

    (define (markdown-check-one path)
      (run-one-task path
        (lambda (p) (list (markdown-check-file-quiet p) #f))
        (lambda (tag info) (list #f #f))
      ) ;run-one-task
    ) ;define

    (define (markdown-check-files files cfg)
      (offenders-from (pool-for-each markdown-check-one files (lambda (file result) #f))
      ) ;offenders-from
    ) ;define

    (define markdown-handler
      (list (cons 'name 'markdown)
        (cons 'label "Markdown")
        (cons 'extensions markdown-extensions)
        (cons 'collect markdown-collect)
        (cons 'format-files markdown-format-files)
        (cons 'format-file format-single-file)
        (cons 'format-directory format-directory)
        (cons 'check-file markdown-check-file)
        (cons 'check-files markdown-check-files)
      ) ;list
    ) ;define

    (register-lang! markdown-handler)

  ) ;begin
) ;define-library
