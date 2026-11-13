;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 登录凭证输入对话框：为选定 provider 输入 API Key、Enter 提交与 Escape 取消。
(define-library (liii pi code modes interactive login-dialog)
  (export make-login-dialog login-dialog? login-dialog-provider
    login-dialog-cursor-position
  ) ;export
  (import (scheme base)
    (liii string)
    (liii unicode)
    (liii tui)
    (liii tui component)
  ) ;import
  (begin

    (define %accent "\x1b;[35m")
    (define %muted "\x1b;[38;2;157;165;169m")
    (define %border "\x1b;[38;2;91;127;200m")
    (define %reset "\x1b;[0m")

    (define (%colored color text)
      (string-append color text %reset)
    ) ;define

    (define-record-type <login-dialog-state>
      (%make-login-dialog-state provider on-submit on-cancel editor)
      login-dialog-state?
      (provider login-dialog-state-provider)
      (on-submit login-dialog-state-on-submit)
      (on-cancel login-dialog-state-on-cancel)
      (editor login-dialog-state-editor login-dialog-state-editor-set!)
    ) ;define-record-type

    (define (%state comp)
      (let ((data (component-data comp)))
        (and (pair? data) (eq? (car data) 'login-dialog) (cadr data))
      ) ;let
    ) ;define

    (define (login-dialog? obj)
      (and (component? obj) (login-dialog-state? (%state obj)))
    ) ;define

    (define (login-dialog-provider comp)
      (login-dialog-state-provider (%state comp))
    ) ;define

    (define (login-dialog-editor comp)
      (login-dialog-state-editor (%state comp))
    ) ;define

    (define (login-dialog-cursor-position comp)
      (let ((ed (login-dialog-editor comp)))
        (list 6 (+ 2 (editor-display-col ed)))
      ) ;let
    ) ;define

    (define (%render comp width)
      (let* ((state (%state comp))
             (provider (login-dialog-state-provider state))
             (border (%colored %border (utf8-make-string (max 1 width) #\─)))
             (ed (login-dialog-state-editor state))
             (input-lines (component-render ed (max 1 (- width 2))))
             (input-line (if (pair? input-lines) (string-append "> " (car input-lines)) "> ")
             ) ;input-line
             (title (string-append "Login to " provider))
             (prompt (string-append "Enter API key for " provider ":"))
             (hint (%colored %muted "  enter to submit · esc to cancel"))
            ) ;
        (list border "" title "" prompt "" input-line "" hint)
      ) ;let*
    ) ;define

    (define (%handle-input comp key)
      (let* ((state (%state comp)) (ed (login-dialog-state-editor state)))
        (case key
         ((escape ctrl-c) ((login-dialog-state-on-cancel state)) #t)
         ((enter)
          (let ((text (string-trim-both (editor-text ed))))
            (if (string-null? text)
              #f
              (begin
               ((login-dialog-state-on-submit state) text)
               #t
              ) ;begin
            ) ;if
          ) ;let
         ) ;
         (else (component-handle-input ed key))
        ) ;case
      ) ;let*
    ) ;define

    (define (make-login-dialog provider on-submit on-cancel)
      (let* ((state (%make-login-dialog-state provider on-submit on-cancel #f))
             (ed (make-text-input "" :multiline #f :focused #t))
             (comp (make-component :render %render :handle-input %handle-input))
            ) ;
        (login-dialog-state-editor-set! state ed)
        (component-data-set! comp (list 'login-dialog state))
        comp
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
