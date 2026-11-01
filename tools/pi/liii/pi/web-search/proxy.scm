;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi web-search proxy)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii json)
    (liii path)
    (liii pi code core tools read)
    (liii pi code config)
  ) ;import
  (export pi-resolve-search-proxy-url
    pi-resolve-search-proxy-alist
    pi-web-search-config-proxy
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

    (define (pi-web-search-config-proxy ws-config)
      (or (%cfg-str ws-config 'httpProxy)
        (%cfg-str ws-config 'proxy)
        (%cfg-str ws-config 'http-proxy)
      ) ;or
    ) ;define

    (define (%provider-proxy-from-store config provider)
      (let ((models-store (%cfg-ref config 'models-store)))
        (and (pair? models-store)
          (let ((entry (assoc provider models-store)))
            (and entry
              (let ((p (or (assoc "httpProxy" (cdr entry)) (assoc 'httpProxy (cdr entry)))))
                (and p
                  (string? (cdr p))
                  (not (string-null? (string-trim-both (cdr p))))
                  (cdr p)
                ) ;and
              ) ;let
            ) ;and
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (%provider-proxy-from-file config-dir provider)
      (and (or (string? config-dir) (path? config-dir))
        (string? provider)
        (let ((p-file (path-join (path config-dir) "models" (string-append provider ".json")))
             ) ;
          (and (path-exists? p-file)
            (catch #t
              (lambda ()
                (let ((data (string->json (path-read-text p-file))))
                  (and (pair? data) (%cfg-str data 'httpProxy))
                ) ;let
              ) ;lambda
              (lambda _ #f)
            ) ;catch
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    (define (%settings-proxy config-dir)
      (catch #t
        (lambda ()
          (let ((s (pi-load-settings config-dir)))
            (and s (%cfg-str s 'http-proxy))
          ) ;let
        ) ;lambda
        (lambda _ #f)
      ) ;catch
    ) ;define

    (define (pi-resolve-search-proxy-alist proxy-url)
      (if
        (and (string? proxy-url) (not (string-null? (string-trim-both proxy-url))))
        (list (cons "http" proxy-url) (cons "https" proxy-url))
        '()
      ) ;if
    ) ;define

    (define (pi-resolve-search-proxy-url config opt-proxy . rest)
      (let* ((ws-config (if (pair? rest) (car rest) #f))
             (explicit-provider? (and (pair? rest) (pair? (cdr rest))))
             (provider (if explicit-provider? (cadr rest) (or (%cfg-str config 'provider) "google"))
             ) ;provider
             (opt-env
               (if (and (pair? rest) (pair? (cdr rest)) (pair? (cddr rest))) (caddr rest) #f)
             ) ;opt-env
             (config-dir (%config-dir-from opt-env))
            ) ;
        (cond
         ((and (string? opt-proxy) (not (string-null? (string-trim-both opt-proxy))))
          opt-proxy
         ) ;
         ((pi-web-search-config-proxy ws-config))
         ((and (not explicit-provider?) (pair? config) (%cfg-str config 'proxy-url))
          (%cfg-str config 'proxy-url)
         ) ;
         ((and (pair? config)
            (equal? (%norm-provider (or (%cfg-str config 'provider) ""))
              (%norm-provider provider)
            ) ;equal?
            (%cfg-str config 'proxy-url)
          ) ;and
          (%cfg-str config 'proxy-url)
         ) ;
         ((%provider-proxy-from-store config provider))
         ((%provider-proxy-from-file config-dir provider))
         ((%env-ref-str "HTTPS_PROXY" opt-env))
         ((%env-ref-str "ALL_PROXY" opt-env))
         ((%settings-proxy config-dir))
         ((and (pair? config) (%cfg-str config 'proxy-url)))
         (else #f)
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
