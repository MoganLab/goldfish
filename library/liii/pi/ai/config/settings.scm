;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai config settings)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii pi ai config retry)
  ) ;import
  (export pi-settings pi-settings? pi-settings-default-provider
    pi-settings-default-model pi-settings-default-thinking-level
    pi-settings-http-proxy pi-settings-retry pi-settings-default-tools
    pi-settings-session-dir pi-settings-hide-thinking-block
    pi-settings-model-thinking-levels pi-settings-enable-skill-commands
    pi-settings-model-thinking-level pi-settings->alist pi-settings->json
    json->pi-settings
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

    (define (%json-dict-pair obj str-key sym-key)
      (let loop
        ((rest obj))
        (cond ((not (pair? rest)) #f)
              ((and (pair? (car rest))
                 (let ((k (car (car rest))))
                   (or (equal? k str-key) (equal? k sym-key))
                 ) ;let
               ) ;and
               (car rest)
              ) ;
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%json-dict-ref obj key default-val)
      (if (not (pair? obj))
        default-val
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (%json-dict-pair obj str-key sym-key))
              ) ;
          (if pair (cdr pair) default-val)
        ) ;let*
      ) ;if
    ) ;define

    (define (%normalize-json-bool val)
      (case val
       ((true) #t)
       ((false) #f)
       (else val)
      ) ;case
    ) ;define

    (define (pi-settings->alist s)
      (if (pair? s)
        s
        (list (cons 'default-provider (pi-settings-default-provider s))
          (cons 'default-model (pi-settings-default-model s))
          (cons 'default-thinking-level (pi-settings-default-thinking-level s))
          (cons 'http-proxy (pi-settings-http-proxy s))
          (cons 'retry (pi-settings-retry s))
          (cons 'default-tools (pi-settings-default-tools s))
          (cons 'session-dir (pi-settings-session-dir s))
          (cons 'hide-thinking-block (pi-settings-hide-thinking-block s))
          (cons 'model-thinking-levels (pi-settings-model-thinking-levels s))
          (cons 'enable-skill-commands (pi-settings-enable-skill-commands s))
        ) ;list
      ) ;if
    ) ;define

    (define (pi-settings->json s)
      (if (pair? s)
        s
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
      ) ;if
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
        (let* ((raw-provider (%json-dict-ref obj "defaultProvider" "google"))
               (default-provider (if (string? raw-provider) raw-provider "google"))
               (raw-model (%json-dict-ref obj "defaultModel" "gemini-3.8-flash"))
               (default-model (if (string? raw-model) raw-model "gemini-3.8-flash"))
               (raw-proxy (%json-dict-ref obj "httpProxy" #f))
               (clean-proxy
                 (and (string? raw-proxy)
                   (not (string-null? (string-trim-both raw-proxy)))
                   raw-proxy
                 ) ;and
               ) ;clean-proxy
               (raw-thinking (%json-dict-ref obj "defaultThinkingLevel" #f))
               (default-thinking (if (string? raw-thinking) raw-thinking #f))
               (raw-session-dir (%json-dict-ref obj "sessionDir" #f))
               (session-dir (if (string? raw-session-dir) raw-session-dir #f))
               (hide-thinking (if (%normalize-json-bool (%json-dict-ref obj "hideThinkingBlock" #f)) #t #f)
               ) ;hide-thinking
               (enable-skills (if (%normalize-json-bool (%json-dict-ref obj "enableSkillCommands" #t)) #t #f)
               ) ;enable-skills
              ) ;
          (pi-settings default-provider
            default-model
            default-thinking
            clean-proxy
            (json->pi-retry-config (%json-dict-ref obj "retry" '()))
            (%parse-tools-list (%json-dict-ref obj "defaultTools" #f))
            session-dir
            hide-thinking
            (%parse-model-thinking-levels (%json-dict-ref obj "modelThinkingLevels" '()))
            enable-skills
          ) ;pi-settings
        ) ;let*
      ) ;if
    ) ;define

    ;; per-model 思考级别覆盖：键为 "provider/modelId"（对齐上游 modelThinkingLevels）
    ;; 本实现的 assoc 对字符串键不按 equal? 比较，故自行遍历
    (define (pi-settings-model-thinking-level settings provider model-id)
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
