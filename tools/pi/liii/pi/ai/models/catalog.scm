;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai models catalog)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii json)
    (liii sort)
    (liii pi ai models builtin)
  ) ;import
  (export pi-catalog-provider-names
    pi-catalog-provider-models
    pi-catalog-merge-provider-models
  ) ;export
  (begin

    (define (%dict-ref obj key default-val)
      (if (not (pair? obj))
        default-val
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (or (assoc str-key obj) (assoc sym-key obj)))
              ) ;
          (if pair (cdr pair) default-val)
        ) ;let*
      ) ;if
    ) ;define

    (define (%read-json-file filepath)
      (and (path-exists? filepath)
        (catch #t (lambda () (string->json (path-read-text filepath))) (lambda args #f))
      ) ;and
    ) ;define

    (define (%models-as-list raw)
      (cond ((vector? raw) (vector->list raw))
            ((list? raw) raw)
            (else '())
      ) ;cond
    ) ;define

    (define (%model-id m)
      (%dict-ref m "id" "")
    ) ;define

    ;; 按模型 id 进行覆盖合并：overlay 优先级高于 base
    (define (pi-catalog-merge-provider-models base overlay)
      (let* ((base-list (%models-as-list base))
             (overlay-list (%models-as-list overlay))
             (overlay-ids (map %model-id overlay-list))
             (filtered-base
               (remove (lambda (m) (member (%model-id m) overlay-ids)) base-list)
             ) ;filtered-base
            ) ;
        (append overlay-list filtered-base)
      ) ;let*
    ) ;define

    (define (%find-entry catalog provider)
      (let* ((p-str (if (symbol? provider) (symbol->string provider) provider))
             (p-sym (if (string? provider) (string->symbol provider) provider))
            ) ;
        (or (assoc p-str catalog) (assoc p-sym catalog))
      ) ;let*
    ) ;define

    ;; 获取指定 provider 的模型列表 (list of alists)
    (define (pi-catalog-provider-models catalog provider)
      (let ((entry (%find-entry catalog provider)))
        (if (not entry)
          '()
          (let ((val (cdr entry)))
            (cond
             ((and (pair? val) (or (assoc "models" val) (assoc 'models val)))
              (%models-as-list (%dict-ref val "models" '()))
             ) ;
             ((or (list? val) (vector? val)) (%models-as-list val))
             (else '())
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%entry-name-str entry)
      (let ((k (car entry)))
        (if (symbol? k) (symbol->string k) (string-copy k))
      ) ;let
    ) ;define

    ;; 收集 alist 中不重复的 provider 名称，字典序排序
    (define (%unique-sorted-names entries)
      (list-sort string<?
        (fold-right
          (lambda (entry names)
            (let ((name (%entry-name-str entry)))
              (if (member name names) names (cons name names))
            ) ;let
          ) ;lambda
          '()
          entries
        ) ;fold-right
      ) ;list-sort
    ) ;define

    ;; 返回 catalog 中全部 provider 名称列表（字符串、字典序排序）
    (define (pi-catalog-provider-names catalog)
      (%unique-sorted-names catalog)
    ) ;define

    (define (%extract-providers-alist data)
      (cond ((not (pair? data)) '())
            (else
              (let ((sub (%dict-ref data "providers" #f)))
                (cond ((pair? sub) sub)
                      (else data)
                ) ;cond
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
