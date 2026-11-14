;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi web-search config)
  (import (scheme base)
    (scheme process-context)
    (liii os)
    (liii string)
    (liii json)
    (liii path)
    (liii pi code core tools read)
    (liii pi code config)
    (liii pi ai auth)
    (liii pi ai template)
    (liii pi web-search proxy)
    (liii pi web-search base-url)
    (liii pi web-search types)
  ) ;import
  (export pi-resolve-google-api-key pi-resolve-search-base-url
    pi-resolve-search-proxy-url pi-resolve-search-proxy-alist
    pi-resolve-search-model-id pi-load-web-search-config
    pi-web-search-config-model pi-web-search-config-proxy
    pi-web-search-config-base-url pi-resolve-web-search-provider
    pi-resolve-deepseek-api-key pi-resolve-deepseek-search-base-url
    make-pi-web-search-config pi-web-search-config?
    pi-web-search-config-provider json->pi-web-search-config
  ) ;export
  (begin

    (define (pi-resolve-google-api-key config opt-key . opt-env)
      (let ((env (if (pair? opt-env) (car opt-env) #f)))
        (cond
         ((string? opt-key)
          (let ((trimmed (string-trim-both opt-key)))
            (if (string-null? trimmed) #f trimmed)
          ) ;let
         ) ;
         ((and (pair? config)
            (let ((p (or (assoc 'provider config) (assoc "provider" config))))
              (or (not p) (equal? (cdr p) "google"))
            ) ;let
            (assoc 'api-key config)
          ) ;and
          (let ((k (cdr (assoc 'api-key config))))
            (if
              (and (string? k) (not (string-null? (string-trim-both k))))
              k
              #f
            ) ;if
          ) ;let
         ) ;
         ((%env-ref-str "GOOGLE_API_KEY" env))
         ((%env-ref-str "GEMINI_API_KEY" env))
         (else
           (catch #t
             (lambda ()
               (let* ((config-dir (%config-dir-from env))
                      (auth (if (pair? env) (pi-load-auth config-dir env) (pi-load-auth config-dir)))
                     ) ;
                 (if (pair? env)
                   (pi-resolve-provider-key "google" auth env)
                   (pi-resolve-provider-key "google" auth)
                 ) ;if
               ) ;let*
             ) ;lambda
             (lambda _ #f)
           ) ;catch
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-resolve-search-model-id config opt-model)
      (cond
       ((and (string? opt-model) (not (string-null? (string-trim-both opt-model))))
        opt-model
       ) ;
       ((and (pair? config)
          (assoc 'model-id config)
          (string? (cdr (assoc 'model-id config)))
        ) ;and
        (cdr (assoc 'model-id config))
       ) ;
       (else "gemini-2.5-flash")
      ) ;cond
    ) ;define

    (define (%cfg-ref config key)
      (and (pair? config)
        (let ((p
                (or (assoc key config)
                  (if (symbol? key) (assoc (symbol->string key) config) #f)
                  (if (string? key) (assoc (string->symbol key) config) #f)
                ) ;or
              ) ;p
             ) ;
          (and (pair? p) (cdr p))
        ) ;let
      ) ;and
    ) ;define

    (define (%cfg-str config key)
      (let ((v (%cfg-ref config key)))
        (if
          (and (string? v) (not (string-null? (string-trim-both v))))
          v
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (%env-ref-str name env)
      ;; env 是 alist 时只查 env（测试可控）；否则查真实环境变量
      (let ((v
              (if (pair? env)
                (let ((p (assoc name env)))
                  (and p (string? (cdr p)) (cdr p))
                ) ;let
                (get-environment-variable name)
              ) ;if
            ) ;v
           ) ;
        (and (string? v) (not (string-null? (string-trim-both v))) v)
      ) ;let
    ) ;define

    ;; JSON 配置文件目录：~/.gf/agent 存在时优先，否则 ~/.pi/agent
    (define (%config-dir-from env)
      (catch #t
        (lambda ()
          (let ((env-dir (%env-ref-str "PI_CODING_AGENT_DIR" env)))
            (if env-dir (pi-abs-path env-dir) (gf-get-config-dir))
          ) ;let
        ) ;lambda
        (lambda _ #f)
      ) ;catch
    ) ;define

    (define (%read-json-file filepath)
      (and (or (string? filepath) (path? filepath))
        (path-exists? (path filepath))
        (catch #t
          (lambda () (string->json (path-read-text (path filepath))))
          (lambda _ #f)
        ) ;catch
      ) ;and
    ) ;define

    (define (%norm-provider s)
      (if (string? s)
        (list->string
          (map
            (lambda (c)
              (let ((n (char->integer c)))
                (if (and (>= n 65) (<= n 90)) (integer->char (+ n 32)) c)
              ) ;let
            ) ;lambda
            (string->list (string-trim-both s))
          ) ;map
        ) ;list->string
        s
      ) ;if
    ) ;define

    ;; 加载 <config-dir>/web-search.json（可用 PI_WEB_SEARCH_CONFIG 指定路径）：
    ;; {"provider": "deepseek|google", "model": "..."}
    ;; 存在时是全局覆盖：任何会话 provider/模型的 web_search 都用它。
    ;; 不存在或解析失败返回 #f。opt-env 提供时只查 opt-env（测试可控）。
    (define (pi-load-web-search-config . opt-env)
      (let* ((env (if (pair? opt-env) (car opt-env) #f))
             (explicit (%env-ref-str "PI_WEB_SEARCH_CONFIG" env))
             (config-dir (%config-dir-from env))
             (default-path
               (and config-dir
                 (catch #t (lambda () (path-join config-dir "web-search.json")) (lambda _ #f))
               ) ;and
             ) ;default-path
            ) ;
        (json->pi-web-search-config (or (%read-json-file explicit) (%read-json-file default-path))
        ) ;json->pi-web-search-config
      ) ;let*
    ) ;define

    (define (pi-web-search-config-model ws-config)
      (if (pi-web-search-config? ws-config)
        (pi-web-search-config-raw-model ws-config)
        (%cfg-str ws-config 'model)
      ) ;if
    ) ;define

    ;; 决定 web_search 用哪个后端：只有 google/deepseek 两个后端，
    ;; 其余 provider 一律回退 google。
    ;; 优先级：显式 opt > web-search.json（全局覆盖） > 环境变量 > config
    (define (pi-resolve-web-search-provider config opt-provider ws-config . opt-env)
      (let* ((env (if (pair? opt-env) (car opt-env) #f))
             (ws-provider (if (pi-web-search-config? ws-config)
                            (pi-web-search-config-provider ws-config)
                            (%cfg-str ws-config 'provider)
                          ) ;if
             ) ;ws-provider
             (raw
               (cond
                ((and (string? opt-provider)
                   (not (string-null? (string-trim-both opt-provider)))
                 ) ;and
                 opt-provider
                ) ;
                (ws-provider)
                ((%cfg-str config 'web-search-provider))
                ((%env-ref-str "GF_PI_WEB_SEARCH_PROVIDER" env))
                ((%cfg-str config 'provider))
                (else "google")
               ) ;cond
             ) ;raw
             (p (%norm-provider raw))
            ) ;
        (if (equal? p "deepseek") "deepseek" "google")
      ) ;let*
    ) ;define

    (define (%deepseek-key-from-auth env)
      ;; 环境变量 + auth.json + deepseek 回退链（claude settings、~/.deepseek.json 等）
      (catch #t
        (lambda ()
          (if (pair? env)
            (pi-resolve-provider-key "deepseek" '() env)
            (pi-resolve-provider-key "deepseek" (pi-load-auth (%config-dir-from env)))
          ) ;if
        ) ;lambda
        (lambda _ #f)
      ) ;catch
    ) ;define

    (define (%config-wants-deepseek? config)
      (equal? (%norm-provider (or (%cfg-str config 'web-search-provider) (%cfg-str config 'provider) "")
              ) ;%norm-provider
        "deepseek"
      ) ;equal?
    ) ;define

    (define (pi-resolve-deepseek-api-key config opt-key . opt-env)
      (let ((env (if (pair? opt-env) (car opt-env) #f)))
        (cond
          ;; 显式传入的 key：字符串但空白时直接失败，不再回退（与 Google 版一致）
          ((string? opt-key)
           (let ((trimmed (string-trim-both opt-key)))
             (if (string-null? trimmed) #f trimmed)
           ) ;let
          ) ;
          ;; config 的 api-key 仅当 provider 是 deepseek 时才可用
          ((and (%config-wants-deepseek? config) (%cfg-str config 'api-key))
           (%cfg-str config 'api-key)
          ) ;
          (else (%deepseek-key-from-auth env))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
