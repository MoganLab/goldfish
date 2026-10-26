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
    (liii raw-string)
    (liii argparse)
    (liii json)
    (liii par)
    (liii timeit)
    (liii go)
    (srfi srfi-133)
    (liii goldtool changed)
    (liii goldtool parallel)
    (liii goldcheck path)
    (liii goldcheck config)
    (liii goldcheck rules)
    (liii goldcheck record-export)
    (liii goldcheck no-glue)
    (liii goldcheck layer)
    (liii goldcheck collect)
    (liii goldcheck only)
    (liii goldcheck cache)
    (liii goldcheck report)
  ) ;import
  (export main check-file-lines check-tree-depth check-no-glue check-file
    check-files make-check-arg-parser format-violation display-help
    violation-report-lines violation-display-hint
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
        (if (file-excluded? file-path (check-rule-excludes cfg "max-file-lines"))
          acc
          (let ((lines-limit (check-rule-limit cfg "max-file-lines")))
            (call-with-values (lambda () (check-file-lines file-path lines-limit))
              (lambda (lines viol)
                (unless viol
                  (rule-cache-touch "max-file-lines" lines-limit file-path)
                ) ;unless
                (if viol (cons viol acc) acc)
              ) ;lambda
            ) ;call-with-values
          ) ;let
        ) ;if
        acc
      ) ;if
    ) ;define

    (define (check-file-rule-depth file-path cfg acc)
      (if (check-rule-enabled? cfg "max-tree-depth")
        (if (file-excluded? file-path (check-rule-excludes cfg "max-tree-depth"))
          acc
          (let ((depth-limit (check-rule-limit cfg "max-tree-depth")))
            (call-with-values (lambda () (check-tree-depth file-path depth-limit))
              (lambda (max-d max-l viol)
                (unless viol
                  (rule-cache-touch "max-tree-depth" depth-limit file-path)
                ) ;unless
                (if viol (cons viol acc) acc)
              ) ;lambda
            ) ;call-with-values
          ) ;let
        ) ;if
        acc
      ) ;if
    ) ;define

    (define (check-file-rule-record-export file-path cfg acc)
      (if (check-rule-enabled? cfg "define-record-type")
        (if (file-excluded? file-path (check-rule-excludes cfg "define-record-type"))
          acc
          (let ((viols (check-record-export file-path)))
            (if (null? viols) (rule-cache-touch "define-record-type" 0 file-path))
            (append acc viols)
          ) ;let
        ) ;if
        acc
      ) ;if
    ) ;define

    (define (check-file-rule-layer file-path cfg acc)
      (if (check-rule-enabled? cfg "layer-order")
        (if (file-excluded? file-path (check-rule-excludes cfg "layer-order"))
          acc
          (append acc (check-layer-order file-path cfg))
        ) ;if
        acc
      ) ;if
    ) ;define

    (define (check-file-rule-no-crlf file-path cfg acc)
      (if (check-rule-enabled? cfg "no-crlf")
        (if (file-excluded? file-path (check-rule-excludes cfg "no-crlf"))
          acc
          (let ((viol (check-no-crlf file-path)))
            (unless viol
              (rule-cache-touch "no-crlf" 0 file-path)
            ) ;unless
            (if viol (cons viol acc) acc)
          ) ;let
        ) ;if
        acc
      ) ;if
    ) ;define

    (define (check-file-rule-no-glue file-path cfg acc)
      (if (check-rule-enabled? cfg "no-glue")
        (if (file-excluded? file-path (check-rule-excludes cfg "no-glue"))
          acc
          (append acc (check-no-glue file-path))
        ) ;if
        acc
      ) ;if
    ) ;define

    (define (check-file file-path cfg)
      "检查单个文件是否违反启用的度量规则。"
      (let* ((acc1 (check-file-rule-lines file-path cfg '()))
             (acc2 (check-file-rule-depth file-path cfg acc1))
             (acc3 (check-file-rule-record-export file-path cfg acc2))
             (acc4 (check-file-rule-layer file-path cfg acc3))
             (acc5 (check-file-rule-no-crlf file-path cfg acc4))
             (acc6 (check-file-rule-no-glue file-path cfg acc5))
            ) ;
        (reverse acc6)
      ) ;let*
    ) ;define

    (define (check-files file-vector cfg)
      "检查文件向量并行执行单文件规则，并检查跨文件 no-glue 唯一归属。"
      (let* ((cfg-text (if cfg (json->string cfg) ""))
             (violations
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
             ) ;violations
            ) ;
        (if (check-rule-enabled? cfg "no-glue")
          (let ((files
                  (filter
                    (lambda (f) (not (file-excluded? f (check-rule-excludes cfg "no-glue"))))
                    (vector->list file-vector)
                  ) ;filter
                ) ;files
               ) ;
            (vector-append violations (list->vector (check-no-glue-duplicates files)))
          ) ;let
          violations
        ) ;if
      ) ;let*
    ) ;define

    (define %all-rule-names
      '("max-file-lines" "max-tree-depth" "define-record-type" "layer-order"
        "no-crlf" "no-glue")
    ) ;define

    (define (%rule-with-enabled rule enabled)
      (cons (cons "enabled" enabled)
        (filter
          (lambda (kv) (not (string=? (car kv) "enabled")))
          rule
        ) ;filter
      ) ;cons
    ) ;define

    (define (single-rule-config cfg rule-name)
      "构造仅启用指定规则的新配置，保留该规则自身的 exclude 等字段。"
      (let ((rules (if cfg (json-ref cfg "rules") '())))
        (let ((new-rules
                (map
                  (lambda (name)
                    (let ((rule (if (json-object? rules) (json-ref rules name) '())))
                      (cons name (%rule-with-enabled rule (string=? name rule-name)))
                    ) ;let
                  ) ;lambda
                  %all-rule-names
                ) ;map
              ) ;new-rules
             ) ;
          (if cfg
            (cons (cons "rules" new-rules)
              (filter
                (lambda (kv) (not (string=? (car kv) "rules")))
                cfg
              ) ;filter
            ) ;cons
            (list (cons "rules" new-rules))
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (enabled-rule-names cfg)
      (filter (lambda (name) (check-rule-enabled? cfg name)) %all-rule-names)
    ) ;define

    (define (run-rule-check files cfg rule-name)
      "执行单个规则检查，返回 (rule-name ok? elapsed violations)。"
      (let ((single-cfg (single-rule-config cfg rule-name))
            (limit (cond ((string=? rule-name "max-file-lines") (check-rule-limit cfg rule-name))
                         ((string=? rule-name "max-tree-depth") (check-rule-limit cfg rule-name))
                         ;; 内容哈希缓存键段：无阈值规则用 0 占位
                         ((string=? rule-name "define-record-type") 0)
                         ((string=? rule-name "no-crlf") 0)
                         ((string=? rule-name "no-glue") #f)
                         (else #f)
                   ) ;cond
            ) ;limit
            (violations '#())
            (elapsed 0.0)
           ) ;
        (set! elapsed
          (timeit
            (lambda ()
              (let ((miss-files
                      (if limit
                        (pool-filter (lambda (f) (not (rule-cache-hit? rule-name limit f))) files)
                        files
                      ) ;if
                    ) ;miss-files
                   ) ;
                (set! violations (check-files (list->vector miss-files) single-cfg))
              ) ;let
            ) ;lambda
            '()
            1
          ) ;timeit
        ) ;set!
        (list rule-name (zero? (vector-length violations)) elapsed violations)
      ) ;let
    ) ;define

    (define (filter-result-files result files)
      (let* ((violations
               (list->vector
                 (filter (lambda (v) (member (cadr v) files string=?))
                   (vector->list (cadddr result))
                 ) ;filter
               ) ;list->vector
             ) ;violations
            ) ;
        (list (car result) (zero? (vector-length violations)) (caddr result) violations)
      ) ;let*
    ) ;define

    (define (only-rule-result parser cfg only files)
      (let* ((rule-files (if (string=? only "no-crlf") (resolve-no-crlf-files parser cfg) files)
             ) ;rule-files
             (incremental-global? (and (string=? only "no-glue") (parser 'changed-since)))
             (analysis-files (if incremental-global? (collect-repo-files) rule-files))
             (result (run-rule-check analysis-files cfg only))
            ) ;
        (if incremental-global? (filter-result-files result rule-files) result)
      ) ;let*
    ) ;define

    (define (configured-rule-result parser cfg rule files no-crlf-files)
      (cond ((string=? rule "no-crlf") (run-rule-check no-crlf-files cfg rule))
            ((and (string=? rule "no-glue") (parser 'changed-since))
             (filter-result-files (run-rule-check (collect-repo-files) cfg rule) files)
            ) ;
            (else (run-rule-check files cfg rule))
      ) ;cond
    ) ;define

    (define (main)
      (set-goldtool-jobs! (go-worker-count))
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
                 (files (resolve-target-files parser))
                ) ;
            (if only
              (let ((result (only-rule-result parser cfg only files)))
                (display-rule-result result)
                (report-and-exit (cadddr result) (caddr result))
              ) ;let
              (let* ((rules (enabled-rule-names cfg0))
                     (no-crlf-files (resolve-no-crlf-files parser cfg0))
                     (results (map (lambda (rule) (configured-rule-result parser cfg0 rule files no-crlf-files))
                                rules
                              ) ;map
                     ) ;results
                     (all-violations (vector-concatenate (map cadddr results)))
                     (total-elapsed (apply + (map caddr results)))
                    ) ;
                (for-each display-rule-result results)
                (report-and-exit all-violations total-elapsed 5)
              ) ;let*
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
