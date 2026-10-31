;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `gf pi config` 只读视图模型：把用户配置目录装配成 ranger 三列所需的数据。
;;
;;   provider 集与顺序 = models/providers.json 白名单顺序
;;   provider 字段     = 内置注册表 < models/<provider>.json
;;   模型字段          = 内置目录 < provider 文件 < models/<provider>/<model>.json
;;   默认标记          = settings.json 的 defaultProvider / defaultModel
;;   key 来源           = 环境变量名 / auth.json（只报来源，不回显密钥）
;;   代理               = provider 文件 > HTTPS_PROXY > ALL_PROXY > settings.json
(define-library (liii pi code modes config state)
  (import (scheme base)
    (scheme file)
    (scheme process-context)
    (liii base)
    (liii list)
    (liii string)
    (liii path)
    (liii json)
    (liii pi ai providers registry)
    (liii pi ai config record)
    (liii pi code config settings)
    (liii pi code config user-fs)
    (liii pi code config user-providers)
    (liii pi code config user-catalog)
  ) ;import
  (export pi-config-state pi-config-state? pi-config-state-dir
    pi-config-state-providers pi-config-state-default-provider
    pi-config-state-default-model pi-config-state-provider-count
    pi-config-state-model-count pi-config-state-enabled-provider-count
    pi-config-state-enabled-model-count pi-config-provider pi-config-provider-id
    pi-config-provider-name pi-config-provider-base-url pi-config-provider-api
    pi-config-provider-proxy pi-config-provider-key-source
    pi-config-provider-models pi-config-provider-default?
    pi-config-provider-file pi-config-provider-enabled? pi-config-model-id
    pi-config-model-name pi-config-model-context pi-config-model-max-tokens
    pi-config-model-reasoning? pi-config-model-file pi-config-model-default?
    pi-config-model-enabled? pi-config-model-detail-rows
  ) ;export
  (begin

    (define-record-type <pi-config-state>
      (%make-config-state dir providers default-provider default-model)
      pi-config-state?
      (dir pi-config-state-dir)
      (providers pi-config-state-providers)
      (default-provider pi-config-state-default-provider)
      (default-model pi-config-state-default-model)
    ) ;define-record-type

    ;; ---------- 小工具 ----------

    (define (%ref entry key)
      (if (pair? entry) (cdr (or (assoc key entry) (cons #f #f))) #f)
    ) ;define

    ;; env 为 #f 时读进程环境；为 alist 时只认 alist（空表即空环境，便于测试）
    (define (%env-get name env)
      (if (eq? env #f)
        (get-environment-variable name)
        (let ((pair (and (pair? env) (assoc name env))))
          (and pair (cdr pair))
        ) ;let
      ) ;if
    ) ;define

    (define (%non-empty str)
      (and (string? str) (not (string-null? (string-trim-both str))) str)
    ) ;define

    ;; ---------- key 来源 ----------

    ;; 返回 "$ENV_NAME" / "auth.json" / #f；只报来源，不回显密钥
    (define (%key-source p-id auth-raw env)
      (let ((hit
              (find (lambda (name) (%non-empty (%env-get name env)))
                (pi-provider-env-keys p-id)
              ) ;find
            ) ;hit
           ) ;
        (cond ((and hit (string? hit)) (string-append "$" hit))
              ((and (pair? auth-raw) (assoc p-id auth-raw)) "auth.json")
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; ---------- 代理链 ----------

    (define (%effective-proxy p-file settings env)
      (or (%non-empty (pi-user-dict-ref p-file "httpProxy" #f))
        (%non-empty (%env-get "HTTPS_PROXY" env))
        (%non-empty (%env-get "ALL_PROXY" env))
        (%non-empty (pi-settings-http-proxy settings))
      ) ;or
    ) ;define

    ;; ---------- 文件存在性 ----------

    (define (%provider-file-rel dir p-id)
      (let ((rel (string-append "models/" p-id ".json")))
        (and (file-exists? (string-append dir "/" rel)) rel)
      ) ;let
    ) ;define

    (define (%model-file-rel dir p-id m-id)
      (let ((rel (string-append "models/" p-id "/" m-id ".json")))
        (and (file-exists? (string-append dir "/" rel)) rel)
      ) ;let
    ) ;define

    ;; 把满足谓词的第一项提到最前（默认 provider / 默认模型都在首行）
    (define (%pin-front items pred?)
      (let ((hit (find pred? items)) (rest (filter (lambda (x) (not (pred? x))) items)))
        (if hit (cons hit rest) items)
      ) ;let
    ) ;define

    ;; ---------- 模型条目 ----------

    (define (%model-entry dir p-id m default-provider default-model)
      (let* ((m-id (%ref m "id"))
             (default? (and (string? default-provider)
                         (string? default-model)
                         (string=? default-provider p-id)
                         (string=? default-model m-id)
                       ) ;and
             ) ;default?
            ) ;
        (list (cons "id" m-id)
          (cons "name" (or (%ref m "name") m-id))
          (cons "contextWindow" (%ref m "contextWindow"))
          (cons "maxTokens" (%ref m "maxTokens"))
          (cons "reasoning" (and (%ref m "reasoning") #t))
          (cons "file" (%model-file-rel dir p-id m-id))
          (cons "default" (and default? #t))
          (cons "enabled" (if (%ref m "enabled") #t #f))
        ) ;list
      ) ;let*
    ) ;define

    ;; ---------- provider 条目 ----------

    (define (%provider-entry config-dir catalog p-id settings auth-raw env
              default-provider default-model
            ) ;%provider-entry
      (let* ((fields (or (pi-user-provider-config catalog p-id) '()))
             (models-raw (pi-user-dict-ref fields "models" '()))
            ) ;
        (list (cons "id" p-id)
          (cons "name" (or (pi-user-dict-ref fields "name" #f) p-id))
          (cons "baseUrl" (pi-user-dict-ref fields "baseUrl" #f))
          (cons "api" (pi-user-dict-ref fields "api" #f))
          (cons "proxy"
            (%effective-proxy (pi-user-provider-file config-dir p-id) settings env)
          ) ;cons
          (cons "keySource" (%key-source p-id auth-raw env))
          (cons "default" (and default-provider (string=? default-provider p-id)))
          (cons "enabled" (if (pi-user-dict-ref fields "enabled" #f) #t #f))
          (cons "file" (%provider-file-rel config-dir p-id))
          (cons "models"
            (%pin-front (map (lambda (m) (%model-entry config-dir p-id m default-provider default-model))
                          models-raw
                        ) ;map
              pi-config-model-default?
            ) ;%pin-front
          ) ;cons
        ) ;list
      ) ;let*
    ) ;define

    ;; ---------- 装配 ----------

    ;; env 缺省 #f：读进程环境变量；显式传 alist 时只用该 alist
    (define* (pi-config-state config-dir (env #f))
      (let* ((dir (path->string (path config-dir)))
             (settings (pi-load-settings (path config-dir)))
             (auth-raw (pi-user-read-json (string-append dir "/auth.json")))
             (catalog (pi-build-config-catalog dir))
             (default-provider (pi-settings-default-provider settings))
             (default-model (pi-settings-default-model settings))
             (providers
               (map (lambda (entry)
                      (%provider-entry dir
                        catalog
                        (car entry)
                        settings
                        auth-raw
                        env
                        default-provider
                        default-model
                      ) ;%provider-entry
                    ) ;lambda
                 catalog
               ) ;map
             ) ;providers
            ) ;
        (%make-config-state dir
          (%pin-front providers pi-config-provider-default?)
          default-provider
          default-model
        ) ;%make-config-state
      ) ;let*
    ) ;define*

    (define (pi-config-state-provider-count st)
      (length (pi-config-state-providers st))
    ) ;define

    (define (pi-config-state-model-count st)
      (apply +
        (map (lambda (p) (length (pi-config-provider-models p)))
          (pi-config-state-providers st)
        ) ;map
      ) ;apply
    ) ;define

    (define (pi-config-state-enabled-provider-count st)
      (length (filter (lambda (p) (pi-config-provider-enabled? p))
                (pi-config-state-providers st)
              ) ;filter
      ) ;length
    ) ;define

    (define (pi-config-state-enabled-model-count st)
      (apply +
        (map
          (lambda (p)
            (if (pi-config-provider-enabled? p)
              (length (filter (lambda (m) (pi-config-model-enabled? m)) (pi-config-provider-models p))
              ) ;length
              0
            ) ;if
          ) ;lambda
          (pi-config-state-providers st)
        ) ;map
      ) ;apply
    ) ;define

    ;; ---------- 访问器 ----------

    (define (pi-config-provider st p-id)
      (find (lambda (p) (equal? (pi-config-provider-id p) p-id))
        (pi-config-state-providers st)
      ) ;find
    ) ;define

    (define (pi-config-provider-id p)
      (%ref p "id")
    ) ;define
    (define (pi-config-provider-name p)
      (%ref p "name")
    ) ;define
    (define (pi-config-provider-base-url p)
      (%ref p "baseUrl")
    ) ;define
    (define (pi-config-provider-api p)
      (%ref p "api")
    ) ;define
    (define (pi-config-provider-proxy p)
      (%ref p "proxy")
    ) ;define
    (define (pi-config-provider-key-source p)
      (%ref p "keySource")
    ) ;define
    (define (pi-config-provider-models p)
      (%ref p "models")
    ) ;define
    (define (pi-config-provider-default? p)
      (%ref p "default")
    ) ;define
    (define (pi-config-provider-file p)
      (%ref p "file")
    ) ;define
    (define (pi-config-provider-enabled? p)
      (%ref p "enabled")
    ) ;define

    (define (pi-config-model-id m)
      (%ref m "id")
    ) ;define
    (define (pi-config-model-name m)
      (%ref m "name")
    ) ;define
    (define (pi-config-model-context m)
      (%ref m "contextWindow")
    ) ;define
    (define (pi-config-model-max-tokens m)
      (%ref m "maxTokens")
    ) ;define
    (define (pi-config-model-reasoning? m)
      (%ref m "reasoning")
    ) ;define
    (define (pi-config-model-file m)
      (%ref m "file")
    ) ;define
    (define (pi-config-model-default? m)
      (%ref m "default")
    ) ;define
    ;; provider 被白名单禁用时，其模型整体不可用
    (define (pi-config-model-enabled? m)
      (%ref m "enabled")
    ) ;define

    ;; ---------- 第三列详情 ----------

    (define (%num->str n)
      (cond ((not n) "—")
            ((not (number? n)) "—")
            ((>= n 1000000) (string-append (number->string (quotient n 1000000)) "M"))
            ((>= n 1000) (string-append (number->string (quotient n 1000)) "K"))
            (else (number->string n))
      ) ;cond
    ) ;define

    ;; 返回 ((标签 值) ...)：详情列内容，值已格式化，不含密钥
    (define (pi-config-model-detail-rows m p)
      (list (list "usable" (if (pi-config-model-enabled? m) "yes" "no (filtered)"))
        (list "id" (or (pi-config-model-id m) ""))
        (list "name" (or (pi-config-model-name m) ""))
        (list "api" (or (pi-config-provider-api p) "—"))
        (list "baseUrl" (or (pi-config-provider-base-url p) "—"))
        (list "context" (%num->str (pi-config-model-context m)))
        (list "maxOutput" (%num->str (pi-config-model-max-tokens m)))
        (list "thinking" (if (pi-config-model-reasoning? m) "yes" "no"))
        (list "proxy" (or (pi-config-provider-proxy p) "—"))
        (list "key" (or (pi-config-provider-key-source p) "missing"))
        (list "file" (or (pi-config-model-file m) "(builtin)"))
      ) ;list
    ) ;define

  ) ;begin
) ;define-library
