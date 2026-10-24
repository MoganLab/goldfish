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

(define-library (liii goldcheck config)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii os)
    (liii path)
    (liii string)
    (liii json)
    (liii list)
    (liii error)
    (liii goldcheck path)
  ) ;import
  (export load-check-config check-config-exists? check-config-goldfish-version
    check-config-rules check-rule-enabled? check-rule-limit check-rule-excludes
    check-rule-suffixes check-rule-paths exclude-entry->path
    path-matches-exclude? file-excluded? validate-check-config!
  ) ;export
  (begin

    (define (validate-rule-name! name)
      (unless (or (string=? name "max-file-lines")
                (string=? name "max-tree-depth")
                (string=? name "define-record-type")
                (string=? name "layer-order")
                (string=? name "no-crlf")
              ) ;or
        (value-error "unknown rule: ~A" name)
      ) ;unless
    ) ;define

    (define (validate-rule-enabled! name r)
      (when (json-contains-key? r "enabled")
        (let ((en (json-ref r "enabled")))
          (unless (boolean? en)
            (type-error "rule ~A enabled must be a boolean: ~S" name en)
          ) ;unless
        ) ;let
      ) ;when
    ) ;define

    (define (validate-rule-limit! name r)
      (when (json-contains-key? r "limit")
        (let ((lim (json-ref r "limit")))
          (unless (integer? lim)
            (type-error "rule ~A limit must be an integer: ~S" name lim)
          ) ;unless
          (unless (> lim 0)
            (value-error "rule ~A limit must be a positive integer: ~S" name lim)
          ) ;unless
        ) ;let
      ) ;when
    ) ;define

    (define (validate-string-array-field! name r field)
      (when (json-contains-key? r field)
        (let ((v (json-ref r field)))
          (unless (json-array? v)
            (type-error "rule ~A ~A must be an array: ~S" name field v)
          ) ;unless
          (for-each (lambda (item)
                      (unless (string? item)
                        (type-error "rule ~A ~A entries must be strings: ~S" name field item)
                      ) ;unless
                    ) ;lambda
            (vector->list v)
          ) ;for-each
        ) ;let
      ) ;when
    ) ;define

    (define (validate-rule-config! name r)
      (unless (json-object? r)
        (type-error "rule configuration for ~A must be an object: ~S" name r)
      ) ;unless
      (validate-rule-enabled! name r)
      (validate-rule-limit! name r)
      (validate-string-array-field! name r "suffix")
      (validate-string-array-field! name r "path")
    ) ;define

    (define (validate-exclude-object! item)
      (unless (json-contains-key? item "path")
        (value-error "exclude entry object missing path field: ~S" item)
      ) ;unless
      (let ((p (json-ref item "path")))
        (unless (string? p)
          (type-error "exclude entry path must be a string: ~S" p)
        ) ;unless
        (when (string=? p "")
          (value-error "exclude path cannot be empty: ~S" item)
        ) ;when
      ) ;let
      (when (json-contains-key? item "reason")
        (let ((r (json-ref item "reason")))
          (unless (string? r)
            (type-error "exclude entry reason must be a string: ~S" r)
          ) ;unless
        ) ;let
      ) ;when
    ) ;define

    (define (validate-exclude-entry! item)
      (cond ((string? item)
             (when (string=? item "")
               (value-error "exclude path cannot be empty: ~S" item)
             ) ;when
            ) ;
            ((json-object? item) (validate-exclude-object! item))
            (else (type-error "exclude entry must be a string or object: ~S" item))
      ) ;cond
    ) ;define

    (define (validate-rules-table! rules)
      (unless (json-object? rules)
        (type-error "rules must be a JSON object: ~S" rules)
      ) ;unless
      (for-each (lambda (name)
                  (validate-rule-name! name)
                  (validate-rule-config! name (json-ref rules name))
                ) ;lambda
        (json-keys rules)
      ) ;for-each
    ) ;define

    (define (validate-exclude-table! ex)
      (unless (json-array? ex)
        (type-error "exclude must be a JSON array: ~S" ex)
      ) ;unless
      (for-each validate-exclude-entry! (vector->list ex))
    ) ;define

    (define (validate-excludes-table! excludes)
      (unless (json-object? excludes)
        (type-error "excludes must be a JSON object: ~S" excludes)
      ) ;unless
      (for-each (lambda (name)
                  (validate-rule-name! name)
                  (validate-exclude-table! (json-ref excludes name))
                ) ;lambda
        (json-keys excludes)
      ) ;for-each
    ) ;define

    (define (validate-check-config! cfg)
      (unless (json-object? cfg)
        (type-error "config must be a JSON object: ~S" cfg)
      ) ;unless
      (when (json-contains-key? cfg "goldfish_version")
        (let ((v (json-ref cfg "goldfish_version")))
          (unless (string? v)
            (type-error "goldfish_version must be a string: ~S" v)
          ) ;unless
        ) ;let
      ) ;when
      (when (json-contains-key? cfg "rules")
        (validate-rules-table! (json-ref cfg "rules"))
      ) ;when
      (when (json-contains-key? cfg "excludes")
        (validate-excludes-table! (json-ref cfg "excludes"))
      ) ;when
      cfg
    ) ;define

    ;; ---- 配置访问器 -----------------------------------------------------

    (define (%rule-field cfg rule-name field)
      (if (or (not cfg) (json-null? cfg))
        #f
        (let ((rules (json-ref cfg "rules")))
          (if (or (json-null? rules) (not (json-object? rules)))
            #f
            (let ((rule (json-ref rules rule-name)))
              (if (or (json-null? rule) (not (json-object? rule)))
                #f
                (let ((v (json-ref rule field)))
                  (if (or (not v) (json-null? v)) #f v)
                ) ;let
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (check-rule-suffixes cfg rule-name)
      "读取规则配置的 suffix 字段（不带点的后缀名数组）。默认 (\"scm\")。"
      (let ((v (%rule-field cfg rule-name "suffix")))
        (if (and v (json-array? v)) (filter string? (vector->list v)) '("scm"))
      ) ;let
    ) ;define

    (define (check-rule-paths cfg rule-name)
      "读取规则配置的 path 字段（目录白名单数组）。默认 () 表示全仓。"
      (let ((v (%rule-field cfg rule-name "path")))
        (if (and v (json-array? v)) (filter string? (vector->list v)) '())
      ) ;let
    ) ;define

    (define (check-config-goldfish-version cfg)
      (if (and cfg (not (json-null? cfg)))
        (json-ref-string cfg "goldfish_version" #f)
        #f
      ) ;if
    ) ;define

    (define (check-config-rules cfg)
      (if (and cfg (not (json-null? cfg))) (json-ref cfg "rules") '(()))
    ) ;define

    (define (check-rule-enabled? cfg rule-name)
      (if (or (not cfg) (json-null? cfg))
        #t
        (let ((rules (json-ref cfg "rules")))
          (if (or (json-null? rules) (not (json-object? rules)))
            #t
            (let ((rule (json-ref rules rule-name)))
              (if (or (json-null? rule) (not (json-object? rule)))
                #t
                (let ((en (json-ref rule "enabled")))
                  (if (boolean? en) en #t)
                ) ;let
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (default-rule-limit rule-name)
      (cond ((string=? rule-name "max-file-lines") 400)
            ((string=? rule-name "max-tree-depth") 15)
            (else 0)
      ) ;cond
    ) ;define

    (define (check-rule-limit cfg rule-name)
      (let ((def (default-rule-limit rule-name)))
        (if (or (not cfg) (json-null? cfg))
          def
          (let ((rules (json-ref cfg "rules")))
            (if (or (json-null? rules) (not (json-object? rules)))
              def
              (let ((rule (json-ref rules rule-name)))
                (if (or (json-null? rule) (not (json-object? rule)))
                  def
                  (let ((lim (json-ref rule "limit")))
                    (if (integer? lim) lim def)
                  ) ;let
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (check-rule-excludes cfg rule-name)
      (if (or (not cfg) (json-null? cfg))
        '()
        (let ((excludes (json-ref cfg "excludes")))
          (if (or (json-null? excludes) (not (json-object? excludes)))
            '()
            (let ((ex (json-ref excludes rule-name)))
              (if (or (json-null? ex) (not (json-array? ex)))
                '()
                (filter identity
                  (map (lambda (entry) (if (string? entry) entry (exclude-entry->path entry)))
                    (vector->list ex)
                  ) ;map
                ) ;filter
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (check-config-exists? . maybe-root)
      (let ((root (if (null? maybe-root) (or (g_project-root) ".") (car maybe-root))))
        (and root (string? root) (file-exists? (string-append root "/gf_check.json")))
      ) ;let
    ) ;define

    (define (find-check-config-file)
      (let ((root (g_project-root)))
        (if (and root (string? root) (not (string=? root "")))
          (let ((candidate (string-append root "/gf_check.json")))
            (if (file-exists? candidate) candidate #f)
          ) ;let
          (let loop
            ((dir "."))
            (let ((candidate (string-append dir "/gf_check.json")))
              (cond ((file-exists? candidate) candidate)
                    ((string=? dir ".") (loop ".."))
                    ((string=? dir "..") (loop "../.."))
                    ((string=? dir "../..") (loop "../../.."))
                    (else #f)
              ) ;cond
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (resolve-config-path maybe-path-or-dir)
      (if (null? maybe-path-or-dir)
        (find-check-config-file)
        (let ((p (car maybe-path-or-dir)))
          (if
            (and (file-exists? p) (not (path-dir? (path p))))
            p
            (string-append p "/gf_check.json")
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (load-check-config . maybe-path-or-dir)
      (let ((target (resolve-config-path maybe-path-or-dir)))
        (if (or (not target) (not (file-exists? target)))
          #f
          (let* ((text (path-read-text (path target))) (cfg (string->json text)))
            (validate-check-config! cfg)
            cfg
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
