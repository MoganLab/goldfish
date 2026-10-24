;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code tools web-search config)
  (import (scheme base)
    (scheme process-context)
    (liii os)
    (liii string)
    (liii pi code tools read)
    (liii pi ai auth)
    (liii pi ai template)
  ) ;import
  (export pi-resolve-google-api-key pi-resolve-search-base-url
    pi-resolve-search-proxy-url pi-resolve-search-proxy-alist
    pi-resolve-search-model-id pi-resolve-web-search-provider
    pi-resolve-deepseek-api-key pi-resolve-deepseek-search-base-url
  ) ;export
  (begin

    (define (pi-resolve-google-api-key config opt-key)
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
       ((get-environment-variable "GOOGLE_API_KEY")
        =>
        (lambda (v) (if (not (string-null? (string-trim-both v))) v #f))
       ) ;
       ((get-environment-variable "GEMINI_API_KEY")
        =>
        (lambda (v) (if (not (string-null? (string-trim-both v))) v #f))
       ) ;
       (else
         (catch #t
           (lambda ()
             (let* ((env-dir (get-environment-variable "PI_CODING_AGENT_DIR"))
                    (agent-dir
                      (if
                        (and (string? env-dir) (not (string-null? (string-trim-both env-dir))))
                        (pi-resolve-path env-dir (getcwd))
                        (pi-resolve-path "~/.pi/agent" (getcwd))
                      ) ;if
                    ) ;agent-dir
                    (auth (pi-load-auth agent-dir))
                   ) ;
               (pi-resolve-provider-key "google" auth)
             ) ;let*
           ) ;lambda
           (lambda _ #f)
         ) ;catch
       ) ;else
      ) ;cond
    ) ;define

    (define (pi-resolve-search-base-url config opt-url)
      (cond
       ((and (string? opt-url) (not (string-null? (string-trim-both opt-url))))
        opt-url
       ) ;
       ((get-environment-variable "GOOGLE_BASE_URL")
        =>
        (lambda (v) (if (not (string-null? (string-trim-both v))) v #f))
       ) ;
       ((get-environment-variable "GEMINI_BASE_URL")
        =>
        (lambda (v) (if (not (string-null? (string-trim-both v))) v #f))
       ) ;
       ((get-environment-variable "GF_PI_BASE_URL")
        =>
        (lambda (v) (if (not (string-null? (string-trim-both v))) v #f))
       ) ;
       ((and (pair? config)
          (let ((p (or (assoc 'provider config) (assoc "provider" config))))
            (or (not p) (equal? (cdr p) "google"))
          ) ;let
          (assoc 'model-meta config)
        ) ;and
        (let* ((meta (cdr (assoc 'model-meta config)))
               (bu (if (pair? meta) (or (assoc 'base-url meta) (assoc "baseURL" meta)) #f))
              ) ;
          (if
            (and bu (string? (cdr bu)) (not (string-null? (string-trim-both (cdr bu)))))
            (cdr bu)
            "https://generativelanguage.googleapis.com/v1beta"
          ) ;if
        ) ;let*
       ) ;
       (else "https://generativelanguage.googleapis.com/v1beta")
      ) ;cond
    ) ;define

    (define (pi-resolve-search-proxy-url config opt-proxy)
      (cond
       ((and (string? opt-proxy) (not (string-null? (string-trim-both opt-proxy))))
        opt-proxy
       ) ;
       ((and (pair? config) (assoc 'proxy-url config)) (cdr (assoc 'proxy-url config)))
       ((get-environment-variable "HTTPS_PROXY")
        =>
        (lambda (v) (if (not (string-null? (string-trim-both v))) v #f))
       ) ;
       ((get-environment-variable "ALL_PROXY")
        =>
        (lambda (v) (if (not (string-null? (string-trim-both v))) v #f))
       ) ;
       (else #f)
      ) ;cond
    ) ;define

    (define (pi-resolve-search-proxy-alist proxy-url)
      (if
        (and (string? proxy-url) (not (string-null? (string-trim-both proxy-url))))
        (list (cons "http" proxy-url) (cons "https" proxy-url))
        '()
      ) ;if
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

    (define (%env-non-empty name opt-env)
      (let ((v
              ;; lookup-env-var 期望 rest 风格的 env（(car custom-env) 才是 alist）
              (lookup-env-var name (if (pair? opt-env) (list opt-env) #f))
            ) ;v
           ) ;
        (and (string? v) (not (string-null? (string-trim-both v))) v)
      ) ;let
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

    ;; 决定 web_search 用哪个后端：只有 google/deepseek 两个后端，
    ;; 其余 provider 一律回退 google。
    ;; 优先级：显式 opt > config 的 web-search-provider > 环境变量 > config 的 provider
    (define (pi-resolve-web-search-provider config opt-provider . opt-env)
      (let* ((env (if (pair? opt-env) (car opt-env) #f))
             (raw
               (cond
                ((and (string? opt-provider)
                   (not (string-null? (string-trim-both opt-provider)))
                 ) ;and
                 opt-provider
                ) ;
                ((%cfg-str config 'web-search-provider))
                ((%env-non-empty "GF_PI_WEB_SEARCH_PROVIDER" env))
                ((%cfg-str config 'provider))
                (else "google")
               ) ;cond
             ) ;raw
             (p (%norm-provider raw))
            ) ;
        (if (equal? p "deepseek") "deepseek" "google")
      ) ;let*
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
          ((and (%cfg-str config 'api-key)
             (equal? (%norm-provider (or (%cfg-str config 'web-search-provider) (%cfg-str config 'provider) "")
                     ) ;%norm-provider
               "deepseek"
             ) ;equal?
           ) ;and
           (%cfg-str config 'api-key)
          ) ;
          ;; 环境变量 + auth.json + deepseek 回退链（claude settings、~/.deepseek.json 等）
          (else
            (catch #t
              (lambda ()
                (let* ((env-dir (lookup-env-var "PI_CODING_AGENT_DIR" (if (pair? env) (list env) #f)))
                       (agent-dir
                         (if
                           (and (string? env-dir) (not (string-null? (string-trim-both env-dir))))
                           (pi-resolve-path env-dir (getcwd))
                           (pi-resolve-path "~/.pi/agent" (getcwd))
                         ) ;if
                       ) ;agent-dir
                       (auth (if (pair? env) '() (pi-load-auth agent-dir)))
                      ) ;
                  (pi-resolve-provider-key "deepseek" '() env)
                ) ;let*
              ) ;lambda
              (lambda _ #f)
            ) ;catch
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (%strip-anthropic-suffix url)
      (if (and (string? url)
            (string-contains? url "/anthropic")
            (string-ends? url "/anthropic")
          ) ;and
        (substring url 0 (- (string-length url) (string-length "/anthropic")))
        url
      ) ;if
    ) ;define

    (define (pi-resolve-deepseek-search-base-url config opt-url . opt-env)
      (let ((env (if (pair? opt-env) (car opt-env) #f)))
        (%strip-anthropic-suffix
          (cond
           ((and (string? opt-url) (not (string-null? (string-trim-both opt-url))))
            opt-url
           ) ;
           ((%env-non-empty "DEEPSEEK_BASE_URL" env))
           ;; config 的 base-url 仅当 provider 是 deepseek 时才可用
           ((and (%cfg-str config 'web-search-base-url)
              (equal? (%norm-provider (or (%cfg-str config 'web-search-provider) (%cfg-str config 'provider) "")
                      ) ;%norm-provider
                "deepseek"
              ) ;equal?
            ) ;and
            (%cfg-str config 'web-search-base-url)
           ) ;
           ((and
              (equal? (%norm-provider (or (%cfg-str config 'web-search-provider) (%cfg-str config 'provider) "")
                      ) ;%norm-provider
                "deepseek"
              ) ;equal?
              (%cfg-ref config 'model-meta)
            ) ;and
            =>
            (lambda (meta)
              (let ((bu (if (pair? meta) (or (%cfg-ref meta 'base-url) (%cfg-ref meta "baseURL")) #f)
                    ) ;bu
                   ) ;
                (if
                  (and (string? bu) (not (string-null? (string-trim-both bu))))
                  bu
                  "https://api.deepseek.com"
                ) ;if
              ) ;let
            ) ;lambda
           ) ;
           (else "https://api.deepseek.com")
          ) ;cond
        ) ;%strip-anthropic-suffix
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
