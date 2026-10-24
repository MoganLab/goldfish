;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi models catalog)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii json)
    (liii sort)
    (liii pi models builtin)
  ) ;import
  (export pi-build-models-catalog pi-catalog-provider-names
    pi-catalog-provider-models pi-catalog-merge-provider-models
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
      (and (path-exists? (path filepath))
        (catch #t
          (lambda () (string->json (path-read-text (path filepath))))
          (lambda args #f)
        ) ;catch
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
      (let loop
        ((rest entries) (acc '()))
        (if (null? rest)
          (list-sort string<? acc)
          (let ((name (%entry-name-str (car rest))))
            (if (member name acc)
              (loop (cdr rest) acc)
              (loop (cdr rest) (cons name acc))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
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

    ;; 构造三层合并模型目录：models.json > models-store.json > 内置目录
    (define (pi-build-models-catalog agent-dir)
      (let* ((store-path (path->string (path-join (path agent-dir) (path "models-store.json")))
             ) ;store-path
             (models-path (path->string (path-join (path agent-dir) (path "models.json"))))
             (store-data (or (%read-json-file store-path) '()))
             (custom-data (%extract-providers-alist (or (%read-json-file models-path) '())))
             ;; 提取所有出现过的 provider 名称
             (all-names
               (%unique-sorted-names
                 (append PI_PROVIDER_BUILTIN_MODELS store-data custom-data)
               ) ;%unique-sorted-names
             ) ;all-names
            ) ;
        (map
          (lambda (p-name)
            (let* ((builtin-m (%models-as-list (%dict-ref PI_PROVIDER_BUILTIN_MODELS p-name '())))
                   (store-entry (%find-entry store-data p-name))
                   (store-m
                     (if store-entry
                       (%models-as-list (%dict-ref (cdr store-entry) "models" '()))
                       '()
                     ) ;if
                   ) ;store-m
                   (custom-entry (%find-entry custom-data p-name))
                   (custom-m
                     (if custom-entry
                       (%models-as-list (%dict-ref (cdr custom-entry) "models" '()))
                       '()
                     ) ;if
                   ) ;custom-m
                   ;; 优先级: custom > store > builtin
                   (merged (pi-catalog-merge-provider-models (pi-catalog-merge-provider-models builtin-m store-m)
                             custom-m
                           ) ;pi-catalog-merge-provider-models
                   ) ;merged
                  ) ;
              (cons p-name (list (cons "models" merged)))
            ) ;let*
          ) ;lambda
          all-names
        ) ;map
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
