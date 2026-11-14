;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code cli args query)
  (import (scheme base) (liii base) (liii list) (liii pi code config query))
  (export pi-cli-args? make-pi-cli-args pi-args-ref pi-args-set
    pi-resolve-timeout
  ) ;export
  (begin

    (define*-record-type <pi-cli-args>
      (make-pi-cli-args (help #f)
        (print #f)
        (mode #f)
        (model #f)
        (provider #f)
        (thinking-level #f)
        (proxy #f)
        (tools #f)
        (exclude-tools #f)
        (no-tools #f)
        (max-turns #f)
        (timeout #f)
        (session-timeout #f)
        (session-dir #f)
        (name #f)
        (no-session #f)
        (continue #f)
        (resume #f)
        (session-file #f)
        (compact #f)
        (no-auto-compact #f)
        (list-models #f)
        (system-prompt #f)
        (append-system-prompt '())
        (skill-paths '())
        (no-skills #f)
        (file-args '())
        (messages '())
        (unknown-flags '())
        (extensions '())
      ) ;make-pi-cli-args
      pi-cli-args?
      (help pi-cli-args-help %set-pi-cli-args-help!)
      (print pi-cli-args-print %set-pi-cli-args-print!)
      (mode pi-cli-args-mode %set-pi-cli-args-mode!)
      (model pi-cli-args-model %set-pi-cli-args-model!)
      (provider pi-cli-args-provider %set-pi-cli-args-provider!)
      (thinking-level pi-cli-args-thinking-level %set-pi-cli-args-thinking-level!)
      (proxy pi-cli-args-proxy %set-pi-cli-args-proxy!)
      (tools pi-cli-args-tools %set-pi-cli-args-tools!)
      (exclude-tools pi-cli-args-exclude-tools %set-pi-cli-args-exclude-tools!)
      (no-tools pi-cli-args-no-tools %set-pi-cli-args-no-tools!)
      (max-turns pi-cli-args-max-turns %set-pi-cli-args-max-turns!)
      (timeout pi-cli-args-timeout %set-pi-cli-args-timeout!)
      (session-timeout pi-cli-args-session-timeout %set-pi-cli-args-session-timeout!)
      (session-dir pi-cli-args-session-dir %set-pi-cli-args-session-dir!)
      (name pi-cli-args-name %set-pi-cli-args-name!)
      (no-session pi-cli-args-no-session %set-pi-cli-args-no-session!)
      (continue pi-cli-args-continue %set-pi-cli-args-continue!)
      (resume pi-cli-args-resume %set-pi-cli-args-resume!)
      (session-file pi-cli-args-session-file %set-pi-cli-args-session-file!)
      (compact pi-cli-args-compact %set-pi-cli-args-compact!)
      (no-auto-compact pi-cli-args-no-auto-compact %set-pi-cli-args-no-auto-compact!)
      (list-models pi-cli-args-list-models %set-pi-cli-args-list-models!)
      (system-prompt pi-cli-args-system-prompt %set-pi-cli-args-system-prompt!)
      (append-system-prompt pi-cli-args-append-system-prompt
        %set-pi-cli-args-append-system-prompt!
      ) ;append-system-prompt
      (skill-paths pi-cli-args-skill-paths %set-pi-cli-args-skill-paths!)
      (no-skills pi-cli-args-no-skills %set-pi-cli-args-no-skills!)
      (file-args pi-cli-args-file-args %set-pi-cli-args-file-args!)
      (messages pi-cli-args-messages %set-pi-cli-args-messages!)
      (unknown-flags pi-cli-args-unknown-flags %set-pi-cli-args-unknown-flags!)
      (extensions pi-cli-args-extensions %set-pi-cli-args-extensions!)
    ) ;define*-record-type

    (define (%clone-pi-cli-args parsed)
      (make-pi-cli-args (pi-cli-args-help parsed)
        (pi-cli-args-print parsed)
        (pi-cli-args-mode parsed)
        (pi-cli-args-model parsed)
        (pi-cli-args-provider parsed)
        (pi-cli-args-thinking-level parsed)
        (pi-cli-args-proxy parsed)
        (pi-cli-args-tools parsed)
        (pi-cli-args-exclude-tools parsed)
        (pi-cli-args-no-tools parsed)
        (pi-cli-args-max-turns parsed)
        (pi-cli-args-timeout parsed)
        (pi-cli-args-session-timeout parsed)
        (pi-cli-args-session-dir parsed)
        (pi-cli-args-name parsed)
        (pi-cli-args-no-session parsed)
        (pi-cli-args-continue parsed)
        (pi-cli-args-resume parsed)
        (pi-cli-args-session-file parsed)
        (pi-cli-args-compact parsed)
        (pi-cli-args-no-auto-compact parsed)
        (pi-cli-args-list-models parsed)
        (pi-cli-args-system-prompt parsed)
        (pi-cli-args-append-system-prompt parsed)
        (pi-cli-args-skill-paths parsed)
        (pi-cli-args-no-skills parsed)
        (pi-cli-args-file-args parsed)
        (pi-cli-args-messages parsed)
        (pi-cli-args-unknown-flags parsed)
        (pi-cli-args-extensions parsed)
      ) ;make-pi-cli-args
    ) ;define

    (define (%pi-cli-args-ref parsed key fallback)
      (case key
       ((help) (pi-cli-args-help parsed))
       ((print) (pi-cli-args-print parsed))
       ((mode) (pi-cli-args-mode parsed))
       ((model) (pi-cli-args-model parsed))
       ((provider) (pi-cli-args-provider parsed))
       ((thinking-level) (pi-cli-args-thinking-level parsed))
       ((proxy) (pi-cli-args-proxy parsed))
       ((tools) (pi-cli-args-tools parsed))
       ((exclude-tools) (pi-cli-args-exclude-tools parsed))
       ((no-tools) (pi-cli-args-no-tools parsed))
       ((max-turns) (pi-cli-args-max-turns parsed))
       ((timeout) (pi-cli-args-timeout parsed))
       ((session-timeout) (pi-cli-args-session-timeout parsed))
       ((session-dir) (pi-cli-args-session-dir parsed))
       ((name) (pi-cli-args-name parsed))
       ((no-session) (pi-cli-args-no-session parsed))
       ((continue) (pi-cli-args-continue parsed))
       ((resume) (pi-cli-args-resume parsed))
       ((session-file) (pi-cli-args-session-file parsed))
       ((compact) (pi-cli-args-compact parsed))
       ((no-auto-compact) (pi-cli-args-no-auto-compact parsed))
       ((list-models) (pi-cli-args-list-models parsed))
       ((system-prompt) (pi-cli-args-system-prompt parsed))
       ((append-system-prompt) (pi-cli-args-append-system-prompt parsed))
       ((skill-paths) (pi-cli-args-skill-paths parsed))
       ((no-skills) (pi-cli-args-no-skills parsed))
       ((file-args) (pi-cli-args-file-args parsed))
       ((messages) (pi-cli-args-messages parsed))
       ((unknown-flags) (pi-cli-args-unknown-flags parsed))
       (else
         (let ((pair (assoc key (pi-cli-args-extensions parsed))))
           (if pair (cdr pair) fallback)
         ) ;let
       ) ;else
      ) ;case
    ) ;define

    (define (%pi-cli-args-set parsed key val)
      (let ((copy (%clone-pi-cli-args parsed)))
        (case key
         ((help) (%set-pi-cli-args-help! copy val))
         ((print) (%set-pi-cli-args-print! copy val))
         ((mode) (%set-pi-cli-args-mode! copy val))
         ((model) (%set-pi-cli-args-model! copy val))
         ((provider) (%set-pi-cli-args-provider! copy val))
         ((thinking-level) (%set-pi-cli-args-thinking-level! copy val))
         ((proxy) (%set-pi-cli-args-proxy! copy val))
         ((tools) (%set-pi-cli-args-tools! copy val))
         ((exclude-tools) (%set-pi-cli-args-exclude-tools! copy val))
         ((no-tools) (%set-pi-cli-args-no-tools! copy val))
         ((max-turns) (%set-pi-cli-args-max-turns! copy val))
         ((timeout) (%set-pi-cli-args-timeout! copy val))
         ((session-timeout) (%set-pi-cli-args-session-timeout! copy val))
         ((session-dir) (%set-pi-cli-args-session-dir! copy val))
         ((name) (%set-pi-cli-args-name! copy val))
         ((no-session) (%set-pi-cli-args-no-session! copy val))
         ((continue) (%set-pi-cli-args-continue! copy val))
         ((resume) (%set-pi-cli-args-resume! copy val))
         ((session-file) (%set-pi-cli-args-session-file! copy val))
         ((compact) (%set-pi-cli-args-compact! copy val))
         ((no-auto-compact) (%set-pi-cli-args-no-auto-compact! copy val))
         ((list-models) (%set-pi-cli-args-list-models! copy val))
         ((system-prompt) (%set-pi-cli-args-system-prompt! copy val))
         ((append-system-prompt) (%set-pi-cli-args-append-system-prompt! copy val))
         ((skill-paths) (%set-pi-cli-args-skill-paths! copy val))
         ((no-skills) (%set-pi-cli-args-no-skills! copy val))
         ((file-args) (%set-pi-cli-args-file-args! copy val))
         ((messages) (%set-pi-cli-args-messages! copy val))
         ((unknown-flags) (%set-pi-cli-args-unknown-flags! copy val))
         (else
           (%set-pi-cli-args-extensions! copy
             (cons (cons key val)
               (filter
                 (lambda (p) (not (eq? (car p) key)))
                 (pi-cli-args-extensions copy)
               ) ;filter
             ) ;cons
           ) ;%set-pi-cli-args-extensions!
         ) ;else
        ) ;case
        copy
      ) ;let
    ) ;define

    (define (pi-args-ref parsed key . default-val)
      (let ((fallback (if (pair? default-val) (car default-val) #f)))
        (if (pi-cli-args? parsed)
          (%pi-cli-args-ref parsed key fallback)
          (let ((pair (assoc key parsed)))
            (if pair (cdr pair) fallback)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (pi-resolve-timeout parsed cfg)
      (let ((t (pi-args-ref parsed 'timeout)))
        (if (and (number? t) (> t 0))
          t
          (let ((cfg-t (pi-cfg-ref cfg 'timeout 600)))
            (if (and (number? cfg-t) (> cfg-t 0)) cfg-t 600)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (pi-args-set parsed key val)
      (if (pi-cli-args? parsed)
        (%pi-cli-args-set parsed key val)
        (cons (cons key val) (filter (lambda (p) (not (eq? (car p) key))) parsed))
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
