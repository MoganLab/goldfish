;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code cli)
  (import (scheme base)
    (scheme process-context)
    (liii raw-string)
    (liii string)
    (liii list)
    (liii sys)
    (liii path)
    (liii pi code cli args)
    (liii pi code config)
    (liii pi code modes)
    (liii pi code core prompt)
    (liii pi code core session-name)
    (liii pi code core session-manager)
    (liii pi code cli help)
    (liii pi ai models)
    (liii pi ai config record)
    (liii pi code modes interactive)
    (liii pi code modes interactive resume)
    (liii pi code core extensions types)
    (liii pi code core extensions registry)
    (liii pi code core extensions discovery)
    (liii pi code core extensions loader)
    (liii pi code core extensions install)
  ) ;import
  (export main display-pi-help read-all-stdin run-goldpi run-pi)
  (begin

    (define (read-all-stdin)
      (let loop
        ((acc '()))
        (let ((chunk (read-string 4096 (current-input-port))))
          (if (or (eof-object? chunk) (= (string-length chunk) 0))
            (let ((trimmed (string-trim-both (apply string-append (reverse acc)))))
              (if (string-null? trimmed) #f trimmed)
            ) ;let
            (loop (cons chunk acc))
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (strip-command-prefix args)
      (if (null? args)
        '()
        (let loop
          ((rest (cdr args)))
          (cond ((null? rest) '())
                ((or (string=? (car rest) "pi") (string-ends? (car rest) "/gf")) (cdr rest))
                (else (loop (cdr rest)))
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (%handle-list-models parsed)
      (let* ((agent-dir (pi-get-agent-dir))
             (cli-provider (pi-args-ref parsed 'provider))
             (target-provider
               (if
                 (and (string? cli-provider)
                   (not (string-null? (string-trim-both cli-provider)))
                 ) ;and
                 cli-provider
                 #f
               ) ;if
             ) ;target-provider
             (models-store (pi-build-models-catalog agent-dir))
             (search-arg (pi-args-ref parsed 'list-models))
             (search-pattern (if (string? search-arg) search-arg #f))
             (rows (pi-list-models models-store target-provider search-pattern))
             (table-text (pi-format-models-table rows search-pattern))
            ) ;
        (display table-text)
        0
      ) ;let*
    ) ;define

    (define (%validate-parsed-args reg parsed initial-msg rest-msgs)
      (let ((validators (extension-registry-validators reg)))
        (for-each
          (lambda (v)
            (catch #t
              (lambda () (v parsed initial-msg rest-msgs))
              (lambda (tag . info) (v parsed))
            ) ;catch
          ) ;lambda
          validators
        ) ;for-each
      ) ;let
    ) ;define

    (define (%has-matching-flow? reg parsed)
      (any (lambda (flow) ((extension-flow-def-predicate flow) parsed))
        (extension-registry-flows reg)
      ) ;any
    ) ;define

    ;; TUI footer 右下角显示实际生效的供应商与模型：与后台 worker 内的解析同源同输入
    ;; （纯本地文件读取，无网络开销），配置解析失败时回退到 CLI 原始参数；
    ;; 返回 (provider model-id thinking-level supports-thinking default-thinking-level
    ;;         agent-dir models default-model)
    ;; 供 make-pi-app 使用（models 为有凭证 provider 的模型列表，供 /model 选择器）
    (define (%tui-footer-info parsed)
      (catch #t
        (lambda ()
          (let* ((cfg (resolve-pi-config parsed (get-environment-variables)))
                 (provider (cdr (or (assoc 'provider cfg) (cons #f "google"))))
                 (model-id (cdr (or (assoc 'model-id cfg) (cons #f "gemini-3.8-flash"))))
                 (tl (cdr (or (assoc 'thinking-level cfg) (cons #f "off"))))
                 (supports (cdr (or (assoc 'supports-thinking cfg) (cons #f #t))))
                 (settings (cdr (or (assoc 'settings cfg) (cons #f #f))))
                 (default-tl (if settings (pi-settings-default-thinking-level settings) tl))
                 (agent-dir (cdr (assoc 'agent-dir cfg)))
                 (models-store (cdr (or (assoc 'models-store cfg) (cons #f '()))))
                 (auth (pi-load-auth agent-dir))
                 (models (pi-usable-models models-store auth))
                 (default-model
                   (and settings
                     (list (cons 'provider (pi-settings-default-provider settings))
                       (cons 'id (pi-settings-default-model settings))
                     ) ;list
                   ) ;and
                 ) ;default-model
                ) ;
            (list provider
              model-id
              tl
              (and supports #t)
              default-tl
              agent-dir
              models
              default-model
            ) ;list
          ) ;let*
        ) ;lambda
        (lambda args
          (list (or (pi-args-ref parsed 'provider) "google")
            (or (pi-args-ref parsed 'model) "gemini-3.8-flash")
            "off"
            #t
            "off"
            (pi-get-agent-dir)
            '()
            #f
          ) ;list
        ) ;lambda
      ) ;catch
    ) ;define

    (define (%tui-session-name parsed)
      (or (pi-args-ref parsed 'name)
        (and (or (pi-args-ref parsed 'continue) (pi-args-ref parsed 'session-file))
          (catch #t
            (lambda ()
              (let* ((cfg (augment-pi-config (resolve-pi-config parsed (get-environment-variables))
                            parsed
                          ) ;augment-pi-config
                     ) ;cfg
                     (cwd (path->string (path-cwd)))
                    ) ;
                (resolve-initial-session-name parsed cfg cwd)
              ) ;let*
            ) ;lambda
            (lambda args #f)
          ) ;catch
        ) ;and
      ) ;or
    ) ;define

    (define (%select-resume-session parsed)
      (let* ((cwd (path->string (path-cwd)))
             (cfg (augment-pi-config (resolve-pi-config parsed (get-environment-variables))
                    parsed
                  ) ;augment-pi-config
             ) ;cfg
            ) ;
        (let-values (((current all) (pi-load-resume-sessions parsed cfg cwd)))
          (pi-run-session-selector current all)
        ) ;let-values
      ) ;let*
    ) ;define

    (define (%run-interactive parsed initial-msg rest-msgs)
      (let* ((info (%tui-footer-info parsed))
             (session-name (%tui-session-name parsed))
             (app
               (make-pi-app
                 :provider               (car info)
                 :model                  (cadr info)
                 :thinking-level         (caddr info)
                 :supports-thinking      (cadddr info)
                 :default-thinking-level (list-ref info 4)
                 :on-save-thinking-level
                 (lambda (level) (pi-save-default-thinking-level! (list-ref info 5) level))
                 :session-name  session-name
                 :models        (list-ref info 6)
                 :default-model (list-ref info 7)
                 :session-file  (pi-args-ref parsed 'session-file)
                 :on-save-model
                 (lambda (provider model-id)
                   (pi-save-default-model! (list-ref info 5) provider model-id)
                 ) ;lambda
               ) ;make-pi-app
             ) ;app
            ) ;
        (pi-app-run app initial-msg rest-msgs)
        0
      ) ;let*
    ) ;define

    (define (%run-pi-main parsed)
      (if (pi-args-ref parsed 'resume)
        (let ((selected (%select-resume-session parsed)))
          (if selected
            (%run-pi-main (pi-args-set (pi-args-set parsed 'resume #f) 'session-file selected)
            ) ;%run-pi-main
            0
          ) ;if
        ) ;let
        (let-values (((initial-msg rest-msgs)
                      (build-initial-prompt (and (not (stdin-tty?)) (read-all-stdin))
                        (process-file-arguments (pi-args-ref parsed 'file-args))
                        (pi-args-ref parsed 'messages)
                      ) ;build-initial-prompt
                     ) ;
                    ) ;
          (let ((reg (or (current-extension-registry) (default-extension-registry))))
            (%validate-parsed-args reg parsed initial-msg rest-msgs)
            (cond ((pi-args-ref parsed 'help) (display-pi-help) 0)
                  ((pi-args-ref parsed 'list-models) (%handle-list-models parsed))
                  ((and (pi-args-ref parsed 'provider) (not (pi-args-ref parsed 'model)))
                   (error 'pi-arg-error
                     (string-append "--provider requires --model (for example: --provider "
                       (pi-args-ref parsed 'provider)
                       " --model <pattern>)"
                     ) ;string-append
                   ) ;error
                  ) ;
                  ((and (string-null? initial-msg)
                     (null? rest-msgs)
                     (not (pi-args-ref parsed 'continue))
                     (not (pi-args-ref parsed 'compact))
                     (not (%has-matching-flow? reg parsed))
                   ) ;and
                   (begin
                     (if (and (stdin-tty?) (stdout-tty?))
                       (%run-interactive parsed "" '())
                       (display-pi-help)
                     ) ;if
                     0
                   ) ;begin
                  ) ;
                  (else
                    (let ((mode (resolve-app-mode (pi-args-ref parsed 'mode)
                                  (pi-args-ref parsed 'print)
                                  (stdin-tty?)
                                  (stdout-tty?)
                                ) ;resolve-app-mode
                          ) ;mode
                         ) ;
                      (if (eq? mode 'interactive)
                        (%run-interactive parsed initial-msg rest-msgs)
                        (with-extension-registry reg
                          (lambda () (dispatch-pi parsed mode initial-msg rest-msgs))
                        ) ;with-extension-registry
                      ) ;if
                    ) ;let
                  ) ;else
            ) ;cond
          ) ;let
        ) ;let-values
      ) ;if
    ) ;define

    (define (%format-catch-error err-type info)
      (cond
       ((and (pair? info) (string? (car info))) (car info))
       ((and (pair? info) (pair? (car info)) (string? (caar info))) (caar info))
       (else (object->string (cons err-type info)))
      ) ;cond
    ) ;define

    (define (%cli-error-use-color?)
      (let ((force-color (get-environment-variable "FORCE_COLOR")))
        (cond
         ((and force-color (not (string=? force-color "0"))) #t)
         ((get-environment-variable "NO_COLOR") #f)
         (else (stderr-tty?))
        ) ;cond
      ) ;let
    ) ;define

    (define (%display-cli-error message)
      (let ((text (string-append "Error: " message)))
        (display-stderr (if (%cli-error-use-color?) (string-append "\x1b;[31m" text "\x1b;[39m") text)
        ) ;display-stderr
      ) ;let
    ) ;define

    (define (%ensure-extensions-loaded!)
      ;; 内置扩展由 args.scm 提前加载；这里仍传入内置目录用于同名冲突仲裁，
      ;; 但不重复返回内置文件。用户级可覆盖内置，项目级不能覆盖内置名称。
      (let* ((reg (default-extension-registry))
             (paths (pi-discover-extensions :include-builtin? #f))
            ) ;
        (pi-load-extensions paths reg)
        reg
      ) ;let*
    ) ;define

    (define (%handle-install-command args)
      (let loop
        ((rem args) (local? #f) (url #f))
        (cond
         ((null? rem)
          (if (not url)
            (begin
              (display-stderr "Error: pi install requires a git URL\nUsage: gf pi install [--local|-l] <git-url>\n"
              ) ;display-stderr
              1
            ) ;begin
            (begin
              (let ((installed (pi-install-extension url :local? local?)))
                (display (string-append "Installed extension to " installed "\n"))
                0
              ) ;let
            ) ;begin
          ) ;if
         ) ;
         ((or (string=? (car rem) "--local") (string=? (car rem) "-l"))
          (loop (cdr rem) #t url)
         ) ;
         ((string-starts? (car rem) "-")
          (display-stderr (string-append "Error: unknown install option: " (car rem) "\n")
          ) ;display-stderr
          1
         ) ;
         ((not url) (loop (cdr rem) local? (car rem)))
         (else (display-stderr (string-append "Error: unexpected extra argument: " (car rem) "\n")
               ) ;display-stderr
           1
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (run-pi raw-args)
      (catch #t
        (lambda ()
          (let ((stripped (strip-command-prefix raw-args)))
            (if (and (pair? stripped) (string=? (car stripped) "install"))
              (%handle-install-command (cdr stripped))
              (begin
                (%ensure-extensions-loaded!)
                (%run-pi-main (parse-pi-args stripped))
              ) ;begin
            ) ;if
          ) ;let
        ) ;lambda
        (lambda (err-type . info)
          (%display-cli-error (%format-catch-error err-type info))
          1
        ) ;lambda
      ) ;catch
    ) ;define
    (define run-goldpi run-pi)

    (define (main)
      (exit (run-pi (command-line)))
    ) ;define

  ) ;begin
) ;define-library
