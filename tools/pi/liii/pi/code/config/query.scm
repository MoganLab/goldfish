;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code config query)
  (import (scheme base) (liii base) (liii list))
  (export pi-config? make-pi-config pi-cfg-ref pi-cfg-set pi-cfg-merge
    pi-cfg->alist
  ) ;export
  (begin

    (define*-record-type <pi-config>
      (make-pi-config (agent-dir #f)
        (config-dir #f)
        (provider "google")
        (provider-api #f)
        (api-key #f)
        (model-id #f)
        (model-meta #f)
        (models-store '())
        (thinking-level "medium")
        (thinking-level-explicit? #f)
        (thinking-level-source 'default)
        (supports-thinking #t)
        (cli-proxy #f)
        (proxy-url #f)
        (tools '())
        (tool-resolver #f)
        (execute-tool-call #f)
        (emit-extension-event #f)
        (settings #f)
        (extras '())
      ) ;make-pi-config
      pi-config?
      (agent-dir %pi-config-agent-dir %set-pi-config-agent-dir!)
      (config-dir %pi-config-config-dir %set-pi-config-config-dir!)
      (provider %pi-config-provider %set-pi-config-provider!)
      (provider-api %pi-config-provider-api %set-pi-config-provider-api!)
      (api-key %pi-config-api-key %set-pi-config-api-key!)
      (model-id %pi-config-model-id %set-pi-config-model-id!)
      (model-meta %pi-config-model-meta %set-pi-config-model-meta!)
      (models-store %pi-config-models-store %set-pi-config-models-store!)
      (thinking-level %pi-config-thinking-level %set-pi-config-thinking-level!)
      (thinking-level-explicit? %pi-config-thinking-level-explicit?
        %set-pi-config-thinking-level-explicit?!
      ) ;thinking-level-explicit?
      (thinking-level-source %pi-config-thinking-level-source
        %set-pi-config-thinking-level-source!
      ) ;thinking-level-source
      (supports-thinking %pi-config-supports-thinking
        %set-pi-config-supports-thinking!
      ) ;supports-thinking
      (cli-proxy %pi-config-cli-proxy %set-pi-config-cli-proxy!)
      (proxy-url %pi-config-proxy-url %set-pi-config-proxy-url!)
      (tools %pi-config-tools %set-pi-config-tools!)
      (tool-resolver %pi-config-tool-resolver %set-pi-config-tool-resolver!)
      (execute-tool-call %pi-config-execute-tool-call
        %set-pi-config-execute-tool-call!
      ) ;execute-tool-call
      (emit-extension-event %pi-config-emit-extension-event
        %set-pi-config-emit-extension-event!
      ) ;emit-extension-event
      (settings %pi-config-settings %set-pi-config-settings!)
      (extras %pi-config-extras %set-pi-config-extras!)
    ) ;define*-record-type

    (define (%clone-pi-config cfg)
      (make-pi-config (%pi-config-agent-dir cfg)
        (%pi-config-config-dir cfg)
        (%pi-config-provider cfg)
        (%pi-config-provider-api cfg)
        (%pi-config-api-key cfg)
        (%pi-config-model-id cfg)
        (%pi-config-model-meta cfg)
        (%pi-config-models-store cfg)
        (%pi-config-thinking-level cfg)
        (%pi-config-thinking-level-explicit? cfg)
        (%pi-config-thinking-level-source cfg)
        (%pi-config-supports-thinking cfg)
        (%pi-config-cli-proxy cfg)
        (%pi-config-proxy-url cfg)
        (%pi-config-tools cfg)
        (%pi-config-tool-resolver cfg)
        (%pi-config-execute-tool-call cfg)
        (%pi-config-emit-extension-event cfg)
        (%pi-config-settings cfg)
        (%pi-config-extras cfg)
      ) ;make-pi-config
    ) ;define

    (define (%pi-config-ref-record cfg key fallback)
      (case key
       ((agent-dir) (%pi-config-agent-dir cfg))
       ((config-dir) (%pi-config-config-dir cfg))
       ((provider) (%pi-config-provider cfg))
       ((provider-api) (%pi-config-provider-api cfg))
       ((api-key) (%pi-config-api-key cfg))
       ((model-id) (%pi-config-model-id cfg))
       ((model-meta) (%pi-config-model-meta cfg))
       ((models-store) (%pi-config-models-store cfg))
       ((thinking-level) (%pi-config-thinking-level cfg))
       ((thinking-level-explicit?) (%pi-config-thinking-level-explicit? cfg))
       ((thinking-level-source) (%pi-config-thinking-level-source cfg))
       ((supports-thinking) (%pi-config-supports-thinking cfg))
       ((cli-proxy) (%pi-config-cli-proxy cfg))
       ((proxy-url) (%pi-config-proxy-url cfg))
       ((tools) (%pi-config-tools cfg))
       ((tool-resolver) (%pi-config-tool-resolver cfg))
       ((execute-tool-call) (%pi-config-execute-tool-call cfg))
       ((emit-extension-event) (%pi-config-emit-extension-event cfg))
       ((settings) (%pi-config-settings cfg))
       (else
         (let ((pair (assoc key (%pi-config-extras cfg))))
           (if pair (cdr pair) fallback)
         ) ;let
       ) ;else
      ) ;case
    ) ;define

    (define (%pi-config-set-record cfg key val)
      (let ((copy (%clone-pi-config cfg)))
        (case key
         ((agent-dir) (%set-pi-config-agent-dir! copy val))
         ((config-dir) (%set-pi-config-config-dir! copy val))
         ((provider) (%set-pi-config-provider! copy val))
         ((provider-api) (%set-pi-config-provider-api! copy val))
         ((api-key) (%set-pi-config-api-key! copy val))
         ((model-id) (%set-pi-config-model-id! copy val))
         ((model-meta) (%set-pi-config-model-meta! copy val))
         ((models-store) (%set-pi-config-models-store! copy val))
         ((thinking-level) (%set-pi-config-thinking-level! copy val))
         ((thinking-level-explicit?) (%set-pi-config-thinking-level-explicit?! copy val))
         ((thinking-level-source) (%set-pi-config-thinking-level-source! copy val))
         ((supports-thinking) (%set-pi-config-supports-thinking! copy val))
         ((cli-proxy) (%set-pi-config-cli-proxy! copy val))
         ((proxy-url) (%set-pi-config-proxy-url! copy val))
         ((tools) (%set-pi-config-tools! copy val))
         ((tool-resolver) (%set-pi-config-tool-resolver! copy val))
         ((execute-tool-call) (%set-pi-config-execute-tool-call! copy val))
         ((emit-extension-event) (%set-pi-config-emit-extension-event! copy val))
         ((settings) (%set-pi-config-settings! copy val))
         (else
           (%set-pi-config-extras! copy
             (cons (cons key val)
               (filter
                 (lambda (p) (not (eq? (car p) key)))
                 (%pi-config-extras copy)
               ) ;filter
             ) ;cons
           ) ;%set-pi-config-extras!
         ) ;else
        ) ;case
        copy
      ) ;let
    ) ;define

    (define (pi-cfg-ref cfg key . default-val)
      (let ((fallback (if (pair? default-val) (car default-val) #f)))
        (cond ((pi-config? cfg) (%pi-config-ref-record cfg key fallback))
              ((pair? cfg) (let ((pair (assoc key cfg))) (if pair (cdr pair) fallback)))
              (else fallback)
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-cfg-set cfg key val)
      (if (pi-config? cfg)
        (%pi-config-set-record cfg key val)
        (let ((base (if (pair? cfg) cfg '())))
          (cons (cons key val) (filter (lambda (p) (not (eq? (car p) key))) base))
        ) ;let
      ) ;if
    ) ;define

    (define (pi-cfg-merge cfg overrides)
      (fold (lambda (pair acc) (pi-cfg-set acc (car pair) (cdr pair))) cfg overrides)
    ) ;define

    (define (pi-cfg->alist cfg)
      (cond
       ((pi-config? cfg)
        (append (list (cons 'agent-dir (%pi-config-agent-dir cfg))
                  (cons 'config-dir (%pi-config-config-dir cfg))
                  (cons 'provider (%pi-config-provider cfg))
                  (cons 'provider-api (%pi-config-provider-api cfg))
                  (cons 'api-key (%pi-config-api-key cfg))
                  (cons 'model-id (%pi-config-model-id cfg))
                  (cons 'model-meta (%pi-config-model-meta cfg))
                  (cons 'models-store (%pi-config-models-store cfg))
                  (cons 'thinking-level (%pi-config-thinking-level cfg))
                  (cons 'thinking-level-explicit? (%pi-config-thinking-level-explicit? cfg))
                  (cons 'thinking-level-source (%pi-config-thinking-level-source cfg))
                  (cons 'supports-thinking (%pi-config-supports-thinking cfg))
                  (cons 'cli-proxy (%pi-config-cli-proxy cfg))
                  (cons 'proxy-url (%pi-config-proxy-url cfg))
                  (cons 'tools (%pi-config-tools cfg))
                  (cons 'tool-resolver (%pi-config-tool-resolver cfg))
                  (cons 'execute-tool-call (%pi-config-execute-tool-call cfg))
                  (cons 'emit-extension-event (%pi-config-emit-extension-event cfg))
                  (cons 'settings (%pi-config-settings cfg))
                ) ;list
          (%pi-config-extras cfg)
        ) ;append
       ) ;
       ((pair? cfg) cfg)
       (else '())
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
