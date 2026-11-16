;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai config settings)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii json)
    (liii list)
    (liii string)
    (liii pi ai config retry)
  ) ;import
  (export pi-settings pi-settings? pi-settings-default-provider
    pi-settings-default-model pi-settings-default-thinking-level
    pi-settings-http-proxy pi-settings-retry pi-settings-default-tools
    pi-settings-session-dir pi-settings-hide-thinking-block
    pi-settings-model-thinking-levels pi-settings-enable-skill-commands
    pi-settings-model-thinking-level pi-settings->json json->pi-settings
  ) ;export
  (begin

    (define*-record-type <pi-settings>
      (pi-settings (default-provider "google")
        (default-model "gemini-3.8-flash")
        (default-thinking-level #f)
        (http-proxy #f)
        (retry (pi-retry-config))
        (default-tools #f)
        (session-dir #f)
        (hide-thinking-block #f)
        (model-thinking-levels '())
        (enable-skill-commands #t)
      ) ;pi-settings
      pi-settings?
      (default-provider pi-settings-default-provider)
      (default-model pi-settings-default-model)
      (default-thinking-level pi-settings-default-thinking-level)
      (http-proxy pi-settings-http-proxy)
      (retry pi-settings-retry)
      (default-tools pi-settings-default-tools)
      (session-dir pi-settings-session-dir)
      (hide-thinking-block pi-settings-hide-thinking-block)
      (model-thinking-levels pi-settings-model-thinking-levels)
      (enable-skill-commands pi-settings-enable-skill-commands)
    ) ;define*-record-type

    (define (pi-settings->json s)
      (unless (pi-settings? s)
        (type-error "pi-settings->json: expected pi-settings" s)
      ) ;unless
      (let ((dp (pi-settings-default-provider s))
            (dm (pi-settings-default-model s))
            (dtl (pi-settings-default-thinking-level s))
            (proxy (pi-settings-http-proxy s))
            (retry (pi-settings-retry s))
            (tools (pi-settings-default-tools s))
            (sdir (pi-settings-session-dir s))
            (levels (pi-settings-model-thinking-levels s))
           ) ;
        (append (if (string? dp) (list (cons "defaultProvider" dp)) '())
          (if (string? dm) (list (cons "defaultModel" dm)) '())
          (if (string? dtl) (list (cons "defaultThinkingLevel" dtl)) '())
          (if (string? proxy) (list (cons "httpProxy" proxy)) '())
          (if retry (list (cons "retry" (pi-retry-config->json retry))) '())
          (if (list? tools) (list (cons "defaultTools" (list->vector tools))) '())
          (if (string? sdir) (list (cons "sessionDir" sdir)) '())
          (list (cons "hideThinkingBlock" (if (pi-settings-hide-thinking-block s) 'true 'false))
          ) ;list
          (if (pair? levels) (list (cons "modelThinkingLevels" levels)) '())
          (list (cons "enableSkillCommands"
                  (if (pi-settings-enable-skill-commands s) 'true 'false)
                ) ;cons
          ) ;list
        ) ;append
      ) ;let
    ) ;define

    (define (%parse-tools-list val)
      (cond ((vector? val) (vector->list val))
            ((list? val) val)
            (else #f)
      ) ;cond
    ) ;define

    (define (%parse-model-thinking-levels val)
      (if (and (pair? val) (every pair? val)) val '())
    ) ;define

    (define (json->pi-settings obj)
      (if (pi-settings? obj)
        obj
        (begin
          (unless (or (json-object? obj) (null? obj))
            (type-error "json->pi-settings: expected json object or pi-settings" obj)
          ) ;unless
          (if (null? obj)
            (pi-settings)
            (let* ((raw-proxy (json-ref-string obj "httpProxy" ""))
                   (clean-proxy
                     (and (not (string-null? (string-trim-both raw-proxy)))
                       raw-proxy
                     ) ;and
                   ) ;clean-proxy
                   (raw-thinking (json-ref obj "defaultThinkingLevel"))
                   (default-thinking (if (string? raw-thinking) raw-thinking #f))
                   (raw-session-dir (json-ref obj "sessionDir"))
                   (session-dir (if (string? raw-session-dir) raw-session-dir #f))
                   (raw-retry (json-ref obj "retry"))
                   (retry
                     (if (or (json-object? raw-retry) (pi-retry-config? raw-retry))
                       (json->pi-retry-config raw-retry)
                       (pi-retry-config)
                     ) ;if
                   ) ;retry
                  ) ;
              (pi-settings (json-ref-string obj "defaultProvider" "google")
                (json-ref-string obj "defaultModel" "gemini-3.8-flash")
                default-thinking
                clean-proxy
                retry
                (and (json-contains-key? obj "defaultTools")
                  (%parse-tools-list (json-ref obj "defaultTools"))
                ) ;and
                session-dir
                (json-ref-boolean obj "hideThinkingBlock" #f)
                (%parse-model-thinking-levels (json-ref obj "modelThinkingLevels"))
                (json-ref-boolean obj "enableSkillCommands" #t)
              ) ;pi-settings
            ) ;let*
          ) ;if
        ) ;begin
      ) ;if
    ) ;define

    ;; per-model 思考级别覆盖：键为 "provider/modelId"（对齐上游 modelThinkingLevels）
    ;; 本实现的 assoc 对字符串键不按 equal? 比较，故自行遍历
    (define (pi-settings-model-thinking-level settings provider model-id)
      (unless (pi-settings? settings)
        (type-error "pi-settings-model-thinking-level: expected pi-settings" settings)
      ) ;unless
      (unless (or (string? provider) (symbol? provider))
        (type-error "pi-settings-model-thinking-level: expected string or symbol provider"
          provider
        ) ;type-error
      ) ;unless
      (unless (string? model-id)
        (type-error "pi-settings-model-thinking-level: expected string model-id"
          model-id
        ) ;type-error
      ) ;unless
      (let* ((provider-str (if (symbol? provider) (symbol->string provider) provider))
             (wanted (string-append provider-str "/" model-id))
             (levels (pi-settings-model-thinking-levels settings))
            ) ;
        (let loop
          ((rest levels))
          (cond ((not (pair? rest)) #f)
                ((not (pair? (car rest))) (loop (cdr rest)))
                ((equal? (car (car rest)) wanted) (cdr (car rest)))
                (else (loop (cdr rest)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
