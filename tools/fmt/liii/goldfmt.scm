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

(define (%goldfmt-common-dirname path-str)
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
    (map (lambda (root) (string-append (%goldfmt-common-dirname root) "/common"))
      *load-path*
    ) ;map
    *load-path*
  ) ;append
) ;set!

(define-library (liii goldfmt)
  (import (liii base)
    (liii sys)
    (liii path)
    (liii string)
    (liii goldfmt scan)
    (liii goldfmt format)
    (liii goldfmt stem)
    (liii goldfmt-lang)
    (liii goldfmt-args)
    (liii goldfmt-changed)
    (liii goldfmt-repo)
    (liii goldfmt-dispatch)
    (liii cpp-fmt)
    (liii stem-fmt)
    (liii goldfmt-parallel)
    (liii go)
    (srfi srfi-13)
  ) ;import
  (export main format-datum format-datum+node format-node format-string
    format-scheme-string format-stem-string can-inline? scan-string
    call-with-stem-mode string-contains all-registered-extensions
    group-files-by-lang format-changed-since check-goldfish-version
    parse-fmt-jobs
  ) ;export
  (begin
    (define format-scheme-string format-string)

    (define (handle-changed-target changed-since path-str extensions cli-excludes dry-run)
      (let ((excludes (append cli-excludes (scheme-config-excludes)))
            (eff-exts (if (extension-option-explicit? (argv)) extensions (all-registered-extensions))
            ) ;eff-exts
           ) ;
        (format-changed-since changed-since path-str eff-exts excludes dry-run)
      ) ;let
    ) ;define

    (define (handle-file-target path-str check-flag dry-run cli-excludes)
      (let ((excludes (append cli-excludes (scheme-config-excludes))))
        (if check-flag
          (dispatch-check-file path-str excludes)
          (dispatch-format-file path-str dry-run excludes)
        ) ;if
      ) ;let
    ) ;define

    (define (handle-dir-target path-str check-flag dry-run extensions cli-excludes)
      (let ((excludes (append cli-excludes (scheme-config-excludes))))
        (if check-flag
          (dispatch-check-directory path-str extensions excludes)
          (dispatch-format-directory path-str extensions excludes dry-run)
        ) ;if
      ) ;let
    ) ;define

    (define (main)
      (ensure-scheme-handler!)
      (let ((parser (make-fmt-arg-parser)))
        (parser :parse-argv (argv))
        (let* ((help-flag (parser 'help))
               (dry-run (parser 'dry-run))
               (check-flag (parser 'check))
               (extensions (parse-extensions (parser 'extension)))
               (changed-since (parser 'changed-since))
               (cli-excludes (parse-excludes (parser 'exclude)))
               (path-str (first-positional parser))
               (dummy-jobs
                 (set-fmt-jobs!
                   (let ((j (normalize-jobs (parser 'jobs))))
                     (if (> j 0) j (go-worker-count))
                   ) ;let
                 ) ;set-fmt-jobs!
               ) ;dummy-jobs
              ) ;
          (cond (help-flag (display-help) #t)
                (changed-since
                 (handle-changed-target changed-since path-str extensions cli-excludes dry-run)
                ) ;
                ((string=? path-str "") (handle-repo-target check-flag))
                ((path-file? (path path-str))
                 (handle-file-target path-str check-flag dry-run cli-excludes)
                ) ;
                ((path-dir? (path path-str))
                 (handle-dir-target path-str check-flag dry-run extensions cli-excludes)
                ) ;
                (else (display (string-append "错误: 路径不存在 - " path-str))
                  (newline)
                  (exit 1)
                ) ;else
          ) ;cond
        ) ;let*
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
