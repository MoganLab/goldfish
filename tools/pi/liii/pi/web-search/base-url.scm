;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi web-search base-url)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii json)
    (liii path)
    (liii pi code core tools read)
    (liii pi code config)
    (liii pi ai config types)
    (liii pi web-search types)
  ) ;import
  (export pi-resolve-search-base-url
    pi-resolve-deepseek-search-base-url
    pi-web-search-config-base-url
  ) ;export
  (begin

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

    (define (%strip-anthropic-suffix url)
      (if (and (string? url)
            (string-contains? url "/anthropic")
            (string-ends? url "/anthropic")
          ) ;and
        (substring url 0 (- (string-length url) (string-length "/anthropic")))
        url
      ) ;if
    ) ;define

    (define (pi-web-search-config-base-url ws-config)
      (and (pi-web-search-config? ws-config)
        (pi-web-search-config-raw-base-url ws-config)
      ) ;and
    ) ;define

    (define (%provider-base-url-from-store config provider)
      (let ((models-store (%cfg-ref config 'models-store))
            (norm-p (%norm-provider provider))
           ) ;
        (and (pair? models-store)
          (let loop
            ((rest models-store))
            (if (null? rest)
              #f
              (let* ((entry (car rest))
                     (key (and (pair? entry) (car entry)))
                     (norm-key
                       (and key (%norm-provider (if (symbol? key) (symbol->string key) key)))
                     ) ;norm-key
                    ) ;
                (if (and norm-key (equal? norm-key norm-p))
                  (or (%cfg-str (cdr entry) "baseUrl")
                    (%cfg-str (cdr entry) 'baseUrl)
                    (%cfg-str (cdr entry) "baseURL")
                    (%cfg-str (cdr entry) 'baseURL)
                    (%cfg-str (cdr entry) "base-url")
                    (%cfg-str (cdr entry) 'base-url)
                  ) ;or
                  (loop (cdr rest))
                ) ;if
              ) ;let*
            ) ;if
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (%provider-base-url-from-file config-dir provider)
      (and (or (string? config-dir) (path? config-dir))
        (string? provider)
        (let* ((norm-p (%norm-provider provider))
               (p-file (path-join (path config-dir) "models" (string-append norm-p ".json")))
              ) ;
          (and (path-exists? p-file)
            (catch #t
              (lambda ()
                (let ((data (string->json (path-read-text p-file))))
                  (and (pair? data)
                    (or (%cfg-str data 'baseUrl) (%cfg-str data 'baseURL) (%cfg-str data 'base-url))
                  ) ;and
                ) ;let
              ) ;lambda
              (lambda _ #f)
            ) ;catch
          ) ;and
        ) ;let*
      ) ;and
    ) ;define

    ;; model-meta 可能是 pi-model-metadata record（会话配置的常态），也可能是 alist；
    ;; record 不能用 pair?/assoc 读取字段，必须用访问器（issue #1172）
    (define (%model-meta-base-url config)
      (let ((meta (%cfg-ref config 'model-meta)))
        (cond
         ((pi-model-metadata? meta)
          (let ((u (pi-model-metadata-base-url meta)))
            (and (string? u) (not (string-null? (string-trim-both u))) u)
          ) ;let
         ) ;
         ((pair? meta)
          (or (%cfg-str meta 'base-url)
            (%cfg-str meta "base-url")
            (%cfg-str meta 'baseURL)
            (%cfg-str meta "baseURL")
            (%cfg-str meta 'baseUrl)
            (%cfg-str meta "baseUrl")
          ) ;or
         ) ;
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%provider-env-base-url provider opt-env)
      (cond ((equal? provider "deepseek") (%env-ref-str "DEEPSEEK_BASE_URL" opt-env))
            ((equal? provider "google")
             (or (%env-ref-str "GOOGLE_BASE_URL" opt-env)
               (%env-ref-str "GEMINI_BASE_URL" opt-env)
             ) ;or
            ) ;
            (else (%env-ref-str (string-append (string-upcase provider) "_BASE_URL") opt-env)
            ) ;else
      ) ;cond
    ) ;define

    (define (%unpack-search-base-url-args config rest)
      (let ((len (length rest)))
        (cond
         ((= len 0) (values #f (or (%cfg-str config 'provider) "google") #f))
         ((= len 1)
          (let ((a (car rest)))
            (if (pi-web-search-config? a)
              (values a (or (%cfg-str config 'provider) "google") #f)
              (values #f (or (%cfg-str config 'provider) "google") a)
            ) ;if
          ) ;let
         ) ;
         ((= len 2)
          (let ((a (car rest)) (b (cadr rest)))
            (if (string? b)
              (values a b #f)
              (values a (or (%cfg-str config 'provider) "google") b)
            ) ;if
          ) ;let
         ) ;
         (else (values (car rest) (cadr rest) (caddr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-resolve-search-base-url config opt-url . rest)
      (let-values (((raw-ws-config raw-provider opt-env)
                    (%unpack-search-base-url-args config rest)
                   ) ;
                  ) ;
        (let* ((ws-config (if (pi-web-search-config? raw-ws-config) raw-ws-config #f))
               (provider (%norm-provider (or raw-provider (%cfg-str config 'provider) "google"))
               ) ;provider
               (config-dir (%config-dir-from opt-env))
               (cfg-prov (%norm-provider (or (%cfg-str config 'provider) "")))
               (prov-match? (or (string-null? cfg-prov) (equal? cfg-prov provider)))
               (raw
                 (cond
                  ((and (string? opt-url) (not (string-null? (string-trim-both opt-url))))
                   opt-url
                  ) ;
                  ((pi-web-search-config-base-url ws-config))
                  ((and prov-match? (%cfg-str config 'web-search-base-url)))
                  ((%provider-env-base-url provider opt-env))
                  ((and prov-match? (%model-meta-base-url config)))
                  ((and prov-match? (%cfg-str config 'base-url)))
                  ((and prov-match? (%cfg-str config 'baseURL)))
                  ((%provider-base-url-from-store config provider))
                  ((%provider-base-url-from-file config-dir provider))
                  ((%env-ref-str "GF_PI_BASE_URL" opt-env))
                  (else (if (equal? provider "deepseek")
                          "https://api.deepseek.com"
                          "https://generativelanguage.googleapis.com/v1beta"
                        ) ;if
                  ) ;else
                 ) ;cond
               ) ;raw
              ) ;
          (if (equal? provider "deepseek") (%strip-anthropic-suffix raw) raw)
        ) ;let*
      ) ;let-values
    ) ;define

    (define (pi-resolve-deepseek-search-base-url config opt-url . rest)
      (let* ((arg1 (if (pair? rest) (car rest) #f))
             (arg2
               (if (and (pair? rest) (pair? (cdr rest))) (cadr rest) #f)
             ) ;arg2
             (ws-config (if (pi-web-search-config? arg1) arg1 #f))
             (opt-env
               (cond ((not (pi-web-search-config? arg1)) arg1)
                     (arg2 arg2)
                     (else #f)
               ) ;cond
             ) ;opt-env
            ) ;
        (pi-resolve-search-base-url config opt-url ws-config "deepseek" opt-env)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
