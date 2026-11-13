;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; /login 交互：解析登录命令、唤起供应商选择器或 Key 输入对话框、持久化凭证至 auth.json
(define-library (liii pi code modes interactive app-login)
  (export pi-app-login-command? pi-app-submit-login!)
  (import (scheme base)
    (liii go)
    (liii string)
    (liii list)
    (liii path)
    (liii tui)
    (liii pi ai auth)
    (liii pi code config)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive views)
    (liii pi code modes interactive login-selector)
    (liii pi code modes interactive login-dialog)
  ) ;import
  (begin

    (define (pi-app-login-command? text)
      (or (string=? text "/login") (string-starts? text "/login "))
    ) ;define

    (define (pi-app-close-login! app)
      (%pi-app-active-selector-set! app #f)
      (editor-set-focused! (pi-app-editor app) #t)
      (pi-app-render! app)
    ) ;define

    (define* (pi-app-save-login-key! app provider key (opt-config-dir #f))
      (let* ((cfg-dir (or opt-config-dir (gf-get-config-dir)))
             (saved-path (catch #t
                           (lambda () (pi-save-auth-key provider key cfg-dir))
                           (lambda (tag . info) #f)
                         ) ;catch
             ) ;saved-path
            ) ;
        (%pi-app-active-selector-set! app #f)
        (editor-set-focused! (pi-app-editor app) #t)
        (if saved-path
          (begin
            (when (pi-app-jobs-chan app)
              (catch #t
                (lambda ()
                  (chan-send! (pi-app-jobs-chan app)
                    (list 'reload_models (path->string (path cfg-dir)))
                    1000
                  ) ;chan-send!
                ) ;lambda
                (lambda args #f)
              ) ;catch
            ) ;when
            (container-add! (%pi-app-chat app)
              (make-assistant-card (string-append "Saved API key for " provider " to " saved-path)
              ) ;make-assistant-card
            ) ;container-add!
          ) ;begin
          (container-add! (%pi-app-chat app)
            (make-tool-card "login"
              '()
              'error
              (string-append "Failed to save API key for " provider)
            ) ;make-tool-card
          ) ;container-add!
        ) ;if
        (pi-app-scroll-bottom! app)
        (pi-app-render! app)
      ) ;let*
    ) ;define*

    (define* (pi-app-show-login-dialog! app provider (opt-config-dir #f))
      (editor-set-focused! (pi-app-editor app) #f)
      (%pi-app-active-selector-set! app
        (make-login-dialog provider
          (lambda (key) (pi-app-save-login-key! app provider key opt-config-dir))
          (lambda () (pi-app-close-login! app))
        ) ;make-login-dialog
      ) ;%pi-app-active-selector-set!
      (pi-app-render! app)
    ) ;define*

    (define* (pi-app-show-login-selector! app (opt-config-dir #f))
      (let ((cfg-dir (or opt-config-dir (gf-get-config-dir))))
        (editor-set-focused! (pi-app-editor app) #f)
        (%pi-app-active-selector-set! app
          (make-login-selector (lambda (provider) (pi-app-show-login-dialog! app provider cfg-dir))
            (lambda () (pi-app-close-login! app))
            :current    (pi-app-provider app)
            :config-dir cfg-dir
          ) ;make-login-selector
        ) ;%pi-app-active-selector-set!
        (pi-app-render! app)
      ) ;let
    ) ;define*

    (define (%split-args str)
      (filter (lambda (s) (not (string-null? s))) (string-split str " "))
    ) ;define

    (define (pi-app-submit-login! app text)
      (editor-set-text! (pi-app-editor app) "")
      (let* ((clean (string-trim-both text)))
        (if (string=? clean "/login")
          (pi-app-show-login-selector! app)
          (let* ((arg-str (string-trim-both (substring clean 6))) (args (%split-args arg-str)))
            (cond ((null? args) (pi-app-show-login-selector! app))
                  ((= (length args) 1) (pi-app-show-login-dialog! app (car args)))
                  (else (pi-app-save-login-key! app (car args) (cadr args)))
            ) ;cond
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
