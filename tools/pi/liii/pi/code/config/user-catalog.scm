;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 用户配置目录的模型目录装配：
;;   provider 集与顺序 = models/providers.json 白名单（缺失时全部内置 provider，字典序）
;;   provider 层字段    = models/<provider>.json
;;   模型条目           = models/<provider>/<model>.json > 内置目录（字段级）
;;
;; 输出 catalog 结构与 models-store 兼容：
;;   ((provider-name ("name" . ..) ("baseUrl" . ..) ("api" . ..)
;;                   ("httpProxy" . ..) ("models" m1 m2 ...)) ...)
(define-library (liii pi code config user-catalog)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii sort)
    (liii pi ai models builtin)
    (liii pi ai providers registry)
    (liii pi code config user-fs)
    (liii pi code config user-providers)
    (liii pi code config user-models)
  ) ;import
  (export pi-build-user-catalog
    pi-build-config-catalog
    pi-user-provider-config
    ;; 白名单加载由 (liii pi code config user-providers) 提供，此处再导出保持入口单一
    pi-load-providers-whitelist
  ) ;export
  (begin

    ;; provider 名（内置注册表优先，缺失时用 id 本身）
    (define (%provider-name p-file p-id)
      (or (pi-user-dict-ref p-file "name" #f) p-id)
    ) ;define

    (define (%builtin-field p-id key)
      (let* ((record (pi-provider-record p-id)) (pair (and record (assq key record))))
        (and pair (cdr pair))
      ) ;let*
    ) ;define

    ;; provider 层 baseUrl/api：用户文件优先，回退内置注册表；api 归一化为全名
    (define (%provider-base-url p-file p-id)
      (or (pi-user-dict-ref p-file "baseUrl" #f) (%builtin-field p-id 'baseUrl))
    ) ;define

    (define (%provider-api p-file p-id)
      (let ((raw (or (pi-user-dict-ref p-file "api" #f) (%builtin-field p-id 'api))))
        (and raw (pi-user-normalize-api raw))
      ) ;let
    ) ;define

    (define (%provider-http-proxy p-file)
      (let ((proxy (pi-user-dict-ref p-file "httpProxy" #f)))
        (if
          (and (string? proxy) (not (string-null? (string-trim-both proxy))))
          (list (cons "httpProxy" proxy))
          '()
        ) ;if
      ) ;let
    ) ;define

    ;; 单个 provider 的目录条目（models 取可用模型；配置视图另用 %config-entry）
    (define (%provider-entry config-dir p-id)
      (let* ((p-file (or (pi-user-provider-file config-dir p-id) '()))
             (base-url (%provider-base-url p-file p-id))
             (api (%provider-api p-file p-id))
             (models (pi-user-provider-models config-dir p-id))
            ) ;
        (cons p-id
          (append (list (cons "name" (%provider-name p-file p-id)))
            (if base-url (list (cons "baseUrl" base-url)) '())
            (if api (list (cons "api" api)) '())
            (%provider-http-proxy p-file)
            (list (cons "models" models))
          ) ;append
        ) ;cons
      ) ;let*
    ) ;define

    ;; provider id 列表：白名单优先，缺失时全部内置 provider（字典序）
    (define (%provider-ids config-dir)
      (or (pi-load-providers-whitelist config-dir)
        (list-sort string<? (map car PI_PROVIDER_BUILTIN_MODELS))
      ) ;or
    ) ;define

    ;; 配置浏览视图：全部内置 provider（白名单顺序在前）+ 每个 provider 的全部模型，
    ;; 各自带 "enabled" 标记，供 `gf pi config` 区分可用/不可用。
    (define (%config-entry config-dir p-id enabled?)
      (let* ((p-file (or (pi-user-provider-file config-dir p-id) '()))
             (base-url (%provider-base-url p-file p-id))
             (api (%provider-api p-file p-id))
            ) ;
        (cons p-id
          (append (list (cons "name" (%provider-name p-file p-id)))
            (if base-url (list (cons "baseUrl" base-url)) '())
            (if api (list (cons "api" api)) '())
            (%provider-http-proxy p-file)
            (list (cons "enabled" (and enabled? #t)))
            (list (cons "models" (pi-user-provider-model-list config-dir p-id)))
          ) ;append
        ) ;cons
      ) ;let*
    ) ;define

    ;; 白名单外的内置 provider（按 id 排序），用于配置视图灰显
    (define (%disabled-provider-ids whitelist)
      (list-sort string<?
        (filter (lambda (p) (not (member p whitelist)))
          (map car PI_PROVIDER_BUILTIN_MODELS)
        ) ;filter
      ) ;list-sort
    ) ;define

    (define (pi-build-config-catalog config-dir)
      (let* ((whitelist (or (pi-load-providers-whitelist config-dir) '()))
             (enabled (map (lambda (p-id) (%config-entry config-dir p-id #t)) whitelist))
             (disabled (map (lambda (p-id) (%config-entry config-dir p-id #f))
                         (%disabled-provider-ids whitelist)
                       ) ;map
             ) ;disabled
            ) ;
        (append enabled disabled)
      ) ;let*
    ) ;define

    ;; 构造用户 catalog（provider 集与顺序由白名单决定）
    (define (pi-build-user-catalog config-dir)
      (map (lambda (p-id) (%provider-entry config-dir p-id))
        (%provider-ids config-dir)
      ) ;map
    ) ;define

    ;; 从 catalog 取 provider 层配置，无则 #f
    (define (pi-user-provider-config catalog p-id)
      (let ((entry (and (pair? catalog) (assoc p-id catalog))))
        (and entry (cdr entry))
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
