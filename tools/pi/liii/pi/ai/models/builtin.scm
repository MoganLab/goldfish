;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 内置模型目录聚合层：数据源已迁移至 (liii pi ai providers *)，
;; 此处保留原接口供 models 目录及外部使用方引用
(define-library (liii pi ai models builtin)
  (import (scheme base) (liii list) (liii pi ai providers registry))
  (export PI_SUPPORTED_PROVIDERS PI_PROVIDER_BUILTIN_MODELS
    pi-builtin-augment-store pi-builtin-augment-all
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

    (define (%models-of entry)
      (let ((raw (%dict-ref (cdr entry) "models" '())))
        (cond ((vector? raw) (vector->list raw))
              ((list? raw) raw)
              (else '())
        ) ;cond
      ) ;let
    ) ;define

    (define (%model-ids models)
      (map (lambda (m) (%dict-ref m "id" "")) models)
    ) ;define

    (define (%missing-models store-models builtin-models)
      (let ((store-ids (%model-ids store-models)))
        (remove (lambda (m) (member (%dict-ref m "id" "") store-ids)) builtin-models)
      ) ;let
    ) ;define

    (define (%provider-entry-name entry)
      (let ((n (car entry)))
        (if (string? n) n (symbol->string n))
      ) ;let
    ) ;define

    (define (%strip-models-field fields)
      (remove
        (lambda (f) (let ((k (car f))) (or (equal? k "models") (eq? k 'models))))
        fields
      ) ;remove
    ) ;define

    (define (%merge-entry-models entry store-models missing)
      (cons (car entry)
        (cons (cons "models" (append store-models missing))
          (%strip-models-field (cdr entry))
        ) ;cons
      ) ;cons
    ) ;define

    (define (%find-provider-entry models-store p-str)
      (find (lambda (e) (equal? (%provider-entry-name e) p-str)) models-store)
    ) ;define

    ;; 把 provider 的内置模型目录补进 models-store。已有条目保留原顺序，
    ;; 缺失的内置模型按键序追加；未知 provider 原样返回。
    (define (pi-builtin-augment-store models-store provider)
      (let* ((p-str (if (symbol? provider) (symbol->string provider) provider))
             (builtin (%dict-ref PI_PROVIDER_BUILTIN_MODELS p-str '()))
            ) ;
        (if (not (pair? builtin))
          models-store
          (let* ((entry (%find-provider-entry models-store p-str))
                 (store-models (if entry (%models-of entry) '()))
                 (missing (%missing-models store-models builtin))
                ) ;
            (cond ((null? missing) models-store)
                  ((not entry) (cons (cons p-str (list (cons "models" builtin))) models-store))
                  (else
                    (map (lambda (e)
                           (if (eq? e entry) (%merge-entry-models entry store-models missing) e)
                         ) ;lambda
                      models-store
                    ) ;map
                  ) ;else
            ) ;cond
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    ;; 对全部内置 provider 执行缺失模型补齐
    (define (pi-builtin-augment-all models-store)
      (fold (lambda (entry acc) (pi-builtin-augment-store acc (car entry)))
        models-store
        PI_PROVIDER_BUILTIN_MODELS
      ) ;fold
    ) ;define

  ) ;begin
) ;define-library
