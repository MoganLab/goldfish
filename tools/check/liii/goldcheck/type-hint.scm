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

;; type-hint 规则：校验 goldfish/hint/ 下 hint 库的结构与签名合法性。
;; 只读源码数据（read 后遍历），不 eval、不加载被检查的库。
;; 校验语义见 hint_spec.md 第 8 节与工具测试文件的说明。

(define-library (liii goldcheck type-hint)
  (import (scheme base)
    (scheme file)
    (scheme write)
    (liii base)
    (liii list)
    (liii string)
    (liii path)
    (liii gfproject config)
    (liii goldcheck path)
    (liii goldcheck type-hint parse)
    (liii goldcheck type-hint util)
    (liii goldcheck type-hint hint-data)
    (liii goldcheck type-hint checks)
    (liii goldcheck type-hint infer)
    (liii goldcheck type-hint env)
  ) ;import
  (export scan-hint-libs check-type-hint check-type-hint-all
    type-hint-status-lines display-type-hint-statuses
  ) ;export
  (begin

    ;; 仓库根下的 goldfish/hint 目录；gfproject-root 带尾斜杠时不再补
    (define (hint-dir)
      (let ((root (gfproject-root)))
        (if (and root (string? root) (not (string=? root "")))
          (if (string-ends? root "/")
            (string-append root "goldfish/hint")
            (string-append root "/goldfish/hint")
          ) ;if
          "goldfish/hint"
        ) ;if
      ) ;let
    ) ;define

    (define (scan-scm-dir dir)
      (if (not (path-dir? (path dir)))
        '()
        (let ((entries (path-list-path (path dir))))
          (let loop
            ((i 0) (acc '()))
            (if (>= i (vector-length entries))
              acc
              (let* ((entry (vector-ref entries i))
                     (s (path->string entry))
                     (bname (path-name (path s)))
                    ) ;
                (cond
                 ((and (path-file? entry) (string-ends? bname ".scm"))
                  (loop (+ i 1) (cons (relativize-path s) acc))
                 ) ;
                 ((and (path-dir? entry)
                    (not (string=? bname ".git"))
                    (not (string=? bname ".cache"))
                    (not (string=? bname ".pi"))
                  ) ;and
                  (loop (+ i 1) (append acc (scan-scm-dir s)))
                 ) ;
                 (else (loop (+ i 1) acc))
                ) ;cond
              ) ;let*
            ) ;if
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    (define (scan-hint-libs)
      "扫描 goldfish/hint/ 下所有 .scm，返回存在 hint 的库文件相对路径列表"
      (append (scan-scm-dir (hint-dir)) (scan-scm-dir "tools/check/hint"))
    ) ;define

    (define (check-type-hint hint-file)
      "检查单个 hint 库文件，返回 (type-hint rel-path line msg) 违规列表"
      (let ((rel (relativize-path hint-file)))
        (let ((info (parse-library-file hint-file)))
          (if (not info)
            '()
            (let* ((qname (query-func-name (lib-name info)))
                   (vars (hint-var-defs info))
                   (query-exists? (and (query-define info qname) #t))
                   (entries (registered-entries info qname))
                  ) ;
              (append (check-a1 rel hint-file info)
                (check-a2 rel info qname)
                (check-a3 rel info)
                (apply append (map (lambda (d) (check-var rel d)) vars))
                (check-a5 rel vars entries query-exists?)
                (check-b rel hint-file entries vars)
                (check-infer hint-file)
              ) ;append
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (type-hint-status-line hint-file violations)
      (let ((info (parse-library-file hint-file)))
        (if (not info)
          #f
          (let* ((orig (orig-lib-path hint-file))
                 (oinfo (and orig (file-exists? orig) (parse-library-file orig)))
                 (env (and oinfo (build-env oinfo hint-file)))
                 (full? (and env (full-coverage? env oinfo) #t))
                 (rel (relativize-path hint-file))
                 (orig-rel (and orig (relativize-path orig)))
                 (failed?
                   (any
                     (lambda (v)
                       (or (string=? (cadr v) rel) (and orig-rel (string=? (cadr v) orig-rel)))
                     ) ;lambda
                     violations
                   ) ;any
                 ) ;failed?
                 (name (lib-name info))
                 (name-text
                   (syms-join (if (eq? (car name) 'hint) (cdr name) name))
                 ) ;name-text
                ) ;
            (string-append "  ["
              (if full? "FULL" "PARTIAL")
              " "
              (if failed? "FAIL" "PASS")
              "\t] ("
              name-text
              ")"
            ) ;string-append
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (type-hint-status-lines hint-files violations)
      (let loop
        ((files hint-files) (acc '()))
        (if (null? files)
          (reverse acc)
          (let ((line (type-hint-status-line (car files) violations)))
            (loop (cdr files) (if line (cons line acc) acc))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (display-type-hint-statuses violations)
      (for-each (lambda (line) (display line) (newline))
        (type-hint-status-lines (scan-hint-libs) violations)
      ) ;for-each
    ) ;define

    (define (check-type-hint-all)
      "扫描 goldfish/hint/ 并检查全部 hint 库，返回违规列表"
      (apply append (map check-type-hint (scan-hint-libs)))
    ) ;define

  ) ;begin
) ;define-library
