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

(define-library (liii cpp-fmt-runner)
  (import (liii base)
    (liii path)
    (liii string)
    (liii subprocess)
    (liii goldfmt-cache)
    (liii goldfmt-config)
    (liii goldfmt-parallel)
  ) ;import
  (export clang-format-binary clang-format-ok? clang-format-hint
    clang-format-run clang-format-run-with cpp-format-one-quiet cpp-fmt-one
    cpp-check-file-quiet cpp-check-one print-cpp-fmt-result
  ) ;export
  (begin

    (define (clang-format-binary . maybe-cfg)
      (let ((cfg (if (null? maybe-cfg) #f (car maybe-cfg))))
        (if cfg (lang-binary 'cpp cfg) "clang-format")
      ) ;let
    ) ;define

    (define (clang-format-ok? . maybe-cfg)
      (let ((cf (apply clang-format-binary maybe-cfg)))
        (let ((sym (string->symbol "clang-format")))
          (run-set! sym cf)
          (= 0 (run '(clang-format "--version")))
        ) ;let
      ) ;let
    ) ;define

    (define (clang-format-hint)
      (display "提示：未找到 clang-format，请安装并确保其在 PATH 中，或在 gf_fmt.json 配置 cpp.binary。"
      ) ;display
      (newline)
    ) ;define

    (define (clang-format-run cfg args . opts)
      (apply clang-format-run-with (clang-format-binary cfg) args opts)
    ) ;define

    (define (clang-format-run-with cf args . opts)
      (let ((sym (string->symbol "clang-format")))
        (run-set! sym cf)
        (apply run (cons (cons sym args) opts))
      ) ;let
    ) ;define

    (define (cpp-format-one-quiet cf path-str)
      (if (fmt-cache-hit? path-str)
        (list 'cached #f)
        (let ((ondisk (path-read-text (path path-str)))
              (rc (clang-format-run-with cf (list "-i" path-str)))
             ) ;
          (if (= rc 0)
            (let ((formatted (path-read-text (path path-str))))
              (if (not (string=? formatted ondisk))
                (begin
                  (fmt-cache-touch path-str)
                  (list 'updated #f)
                ) ;begin
                (begin
                  (fmt-cache-touch path-str)
                  (list 'unchanged #f)
                ) ;begin
              ) ;if
            ) ;let
            (list 'failed #f)
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (cpp-fmt-one cf path)
      (run-one-task path
        (lambda (p) (cpp-format-one-quiet cf p))
        (lambda (tag info) (list 'failed #f))
      ) ;run-one-task
    ) ;define

    (define (print-cpp-fmt-result file result)
      (when (eq? (cadr result) 'updated)
        (display (string-append "  Updated: " file))
        (newline)
      ) ;when
    ) ;define

    (define (cpp-check-file-quiet cf path)
      (if (fmt-cache-hit? path)
        #t
        (let ((rc (clang-format-run-with cf
                    (list "--dry-run" "--Werror" path)
                    :stdout 'discard
                    :stderr 'discard
                  ) ;clang-format-run-with
              ) ;rc
             ) ;
          (when (= rc 0)
            (fmt-cache-touch path)
          ) ;when
          (= rc 0)
        ) ;let
      ) ;if
    ) ;define

    (define (cpp-check-one cf path)
      (run-one-task path
        (lambda (p) (list (cpp-check-file-quiet cf p) #f))
        (lambda (tag info) (list #f #f))
      ) ;run-one-task
    ) ;define

  ) ;begin
) ;define-library
