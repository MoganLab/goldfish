;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 登录供应商选择器：展示各 provider 列表、配置状态（✓ configured / • not configured）、
;; 模糊搜索与选择流转。
(define-library (liii pi code modes interactive login-selector)
  (export make-login-selector login-selector? login-selector-cursor-position)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii unicode)
    (liii path)
    (liii tui)
    (liii tui component)
    (liii pi ai auth)
    (liii pi ai providers registry)
    (liii pi code config)
  ) ;import
  (begin

    (define %accent "\x1b;[35m")
    (define %muted "\x1b;[38;2;157;165;169m")
    (define %border "\x1b;[38;2;91;127;200m")
    (define %success "\x1b;[32m")
    (define %reset "\x1b;[0m")

    (define (%colored color text)
      (string-append color text %reset)
    ) ;define

    (define-record-type <login-selector-state>
      (%make-login-selector-state current providers config-dir on-select
        on-cancel search list
      ) ;%make-login-selector-state
      login-selector-state?
      (current login-selector-state-current)
      (providers login-selector-state-providers)
      (config-dir login-selector-state-config-dir)
      (on-select login-selector-state-on-select)
      (on-cancel login-selector-state-on-cancel)
      (search login-selector-state-search login-selector-state-search-set!)
      (list login-selector-state-list login-selector-state-list-set!)
    ) ;define-record-type

    (define (%state comp)
      (let ((data (component-data comp)))
        (and (pair? data) (eq? (car data) 'login-selector) (cadr data))
      ) ;let
    ) ;define

    (define (login-selector? obj)
      (and (component? obj) (login-selector-state? (%state obj)))
    ) ;define

    (define (login-selector-list comp)
      (login-selector-state-list (%state comp))
    ) ;define

    (define (login-selector-search comp)
      (login-selector-state-search (%state comp))
    ) ;define

    (define (login-selector-cursor-position comp)
      (let ((search (login-selector-search comp)))
        (list 6 (+ 2 (editor-display-col search)))
      ) ;let
    ) ;define

    (define (%provider-status provider config-dir)
      (let* ((creds (catch #t (lambda () (pi-load-auth config-dir)) (lambda args '())))
             (key (catch #t (lambda () (pi-resolve-provider-key provider creds)) (lambda args #f))
             ) ;key
            ) ;
        (if
          (and (string? key) (not (string-null? (string-trim-both key))))
          "✓ configured"
          "• not configured"
        ) ;if
      ) ;let*
    ) ;define

    (define (%provider-items state)
      (let ((cfg-dir (login-selector-state-config-dir state)))
        (map
          (lambda (provider)
            (let* ((status (%provider-status provider cfg-dir))
                   (configured? (string-starts? status "✓"))
                  ) ;
              (list (cons 'value provider)
                (cons 'label provider)
                (cons 'description
                  (if configured? (%colored %success status) (%colored %muted status))
                ) ;cons
              ) ;list
            ) ;let*
          ) ;lambda
          (login-selector-state-providers state)
        ) ;map
      ) ;let
    ) ;define

    (define (%build-list state items preselect)
      (let ((lst
              (make-select-list items
                (max 1 (min 8 (length items)))
                :min-primary-column-width 16
                :max-primary-column-width 32
                :selected-text    (lambda (text) (%colored %accent text))
                :description-text (lambda (text) text)
                :scroll-text      (lambda (text) (%colored %muted text))
                :no-match-text    (lambda (text) (%colored %muted text))
                :on-select
                (lambda (item)
                 ((login-selector-state-on-select state) (cdr (assoc 'value item)))
                ) ;lambda
                :on-cancel (lambda () ((login-selector-state-on-cancel state)))
              ) ;make-select-list
            ) ;lst
            (index
              (list-index
                (lambda (item) (equal? (cdr (assoc 'value item)) preselect))
                items
              ) ;list-index
            ) ;index
           ) ;
        (when index
          (select-list-set-selected-index! lst index)
        ) ;when
        lst
      ) ;let
    ) ;define

    (define (%refresh-list! state)
      (let* ((old-list (login-selector-state-list state))
             (selected (and old-list (select-list-selected-item old-list)))
             (preselect (and selected (cdr (assoc 'value selected))))
             (query (editor-text (login-selector-state-search state)))
             (filtered
               (fuzzy-filter (%provider-items state)
                 query
                 (lambda (item) (cdr (assoc 'value item)))
               ) ;fuzzy-filter
             ) ;filtered
            ) ;
        (login-selector-state-list-set! state (%build-list state filtered preselect))
      ) ;let*
    ) ;define

    (define (%render comp width)
      (let* ((state (%state comp))
             (border (%colored %border (utf8-make-string (max 1 width) #\─)))
             (search (login-selector-state-search state))
             (search-lines (component-render search (max 1 (- width 2))))
             (search-line (if (pair? search-lines) (string-append "> " (car search-lines)) "> ")
             ) ;search-line
             (hint (%colored %muted "  enter to select · esc to cancel"))
            ) ;
        (append (list border "" "Login Provider" ""
                  "Select a provider to configure authentication" "" search-line
                  ""
                ) ;list
          (component-render (login-selector-state-list state) width)
          (list "" hint)
        ) ;append
      ) ;let*
    ) ;define

    (define (%handle-input comp key)
      (let ((state (%state comp)))
        (cond ((memq key '(up down enter escape ctrl-c))
               (component-handle-input (login-selector-state-list state) key)
              ) ;
              (else (component-handle-input (login-selector-state-search state) key))
        ) ;cond
      ) ;let
    ) ;define

    (define* (make-login-selector on-select
               on-cancel
               (current #f)
               (config-dir #f)
               (providers #f)
             ) ;make-login-selector
      (let* ((actual-cfg-dir (or config-dir (gf-get-config-dir)))
             (actual-providers (or providers PI_SUPPORTED_PROVIDERS))
             (state (%make-login-selector-state current actual-providers
                      actual-cfg-dir on-select on-cancel #f #f
                    ) ;%make-login-selector-state
             ) ;state
             (search (make-text-input ""
                       :multiline #f
                       :focused   #t
                       :on-change (lambda (ed text) (%refresh-list! state))
                     ) ;make-text-input
             ) ;search
             (comp (make-component :render %render :handle-input %handle-input))
            ) ;
        (login-selector-state-search-set! state search)
        (login-selector-state-list-set! state
          (%build-list state (%provider-items state) current)
        ) ;login-selector-state-list-set!
        (component-data-set! comp (list 'login-selector state))
        comp
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
