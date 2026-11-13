;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; --resume 启动选择器：当前/全部会话、模糊搜索、命名过滤与路径显示。
(define-library (liii pi code modes interactive session-selector)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii sort)
    (liii unicode)
    (liii error)
    (liii date-time)
    (liii tui)
    (liii tui component)
    (liii pi code core session-list)
    (liii pi code modes interactive session-selector-time)
    (liii pi code modes interactive session-selector-search)
  ) ;import
  (export make-session-selector session-selector? session-selector-scope
    session-selector-sort-mode session-selector-name-filter
    session-selector-show-path? session-selector-visible-sessions
    session-selector-search session-selector-selected-index
    session-selector-format-age session-selector-cursor-position
    session-selector-step-index!
  ) ;export
  (begin

    (define %accent "\x1b;[35m")
    (define %muted "\x1b;[38;2;157;165;169m")
    (define %border "\x1b;[38;2;91;127;200m")
    (define %reset "\x1b;[0m")
    (define (%colored color text)
      (string-append color text %reset)
    ) ;define

    (define-record-type <session-selector-state>
      (%make-state current all on-select on-cancel scope sort-mode name-filter
        show-path? search visible selected text-cache error-msg incremental?
      ) ;%make-state
      state?
      (current state-current)
      (all state-all state-all-set!)
      (on-select state-on-select)
      (on-cancel state-on-cancel)
      (scope state-scope state-scope-set!)
      (sort-mode state-sort-mode state-sort-mode-set!)
      (name-filter state-name-filter state-name-filter-set!)
      (show-path? state-show-path? state-show-path?-set!)
      (search state-search state-search-set!)
      (visible state-visible state-visible-set!)
      (selected state-selected state-selected-set!)
      ;; 会话在选择器生命周期内不变，消毒并小写化的搜索文本按会话缓存，
      ;; 避免每次按键对大文本重复小写化
      (text-cache state-text-cache state-text-cache-set!)
      (error-msg state-error-msg state-error-msg-set!)
      (incremental? state-incremental?)
    ) ;define-record-type

    (define (%state comp)
      (let ((data (component-data comp)))
        (and (pair? data) (eq? (car data) 'session-selector) (cadr data))
      ) ;let
    ) ;define
    (define (session-selector? obj)
      (and (component? obj) (state? (%state obj)))
    ) ;define
    (define (session-selector-scope comp)
      (state-scope (%state comp))
    ) ;define
    (define (session-selector-sort-mode comp)
      (state-sort-mode (%state comp))
    ) ;define
    (define (session-selector-name-filter comp)
      (state-name-filter (%state comp))
    ) ;define
    (define (session-selector-show-path? comp)
      (state-show-path? (%state comp))
    ) ;define
    (define (session-selector-visible-sessions comp)
      (state-visible (%state comp))
    ) ;define
    (define (session-selector-search comp)
      (state-search (%state comp))
    ) ;define
    (define (session-selector-selected-index comp)
      (state-selected (%state comp))
    ) ;define

    ;; 搜索输入框光标位置：渲染时搜索行位于第 6 行（0 基），
    ;; 行首为 "> " 前缀，故列偏移为 2 + 搜索文本显示宽度
    ;; （与 model/thinking 选择器同构，供 TUI 内嵌渲染定位硬件光标）
    (define (session-selector-cursor-position comp)
      (let ((search (state-search (%state comp))))
        (list 6 (+ 2 (editor-display-col search)))
      ) ;let
    ) ;define

    (define (%source state)
      (if (eq? (state-scope state) 'current)
        (state-current state)
        (let ((all (state-all state)))
          (if (procedure? all)
            (let ((loaded (all)))
              (state-all-set! state loaded)
              loaded
            ) ;let
            all
          ) ;if
        ) ;let
      ) ;if
    ) ;define
    (define (%format-error tag info)
      (let ((fallback (if (symbol? tag) (symbol->string tag) "unknown error"))
            (msg (catch #t (lambda () (catch-error->string tag info)) (lambda _ #f)))
           ) ;
        (if (and (string? msg) (not (string-contains? msg "#1="))) msg fallback)
      ) ;let
    ) ;define

    (define (%refresh! state)
      (catch #t
        (lambda ()
          (let* ((source (%source state))
                 (named
                   (if (eq? (state-name-filter state) 'named)
                     (filter (lambda (s) (pi-session-info-name s)) source)
                     source
                   ) ;if
                 ) ;named
                 (query
                   (string-trim-both (utf8-replace-invalid (editor-text (state-search state))))
                 ) ;query
                 (matched (session-selector-filter-sessions (state-text-cache state)
                            named
                            query
                            (state-sort-mode state)
                            (state-incremental? state)
                          ) ;session-selector-filter-sessions
                 ) ;matched
                 (visible
                   (if (or (string-null? query) (eq? (state-sort-mode state) 'relevance))
                     matched
                     (list-sort pi-session-info-newer? matched)
                   ) ;if
                 ) ;visible
                 (last (max 0 (- (length visible) 1)))
                ) ;
            (state-visible-set! state visible)
            (state-selected-set! state (max 0 (min (state-selected state) last)))
            (state-error-msg-set! state #f)
          ) ;let*
        ) ;lambda
        (lambda (tag . info) (state-error-msg-set! state (%format-error tag info)))
      ) ;catch
    ) ;define

    (define (%one-line text)
      (string-trim-both (string-replace (string-replace (string-replace text "\r" " ") "\n" " ")
                          "\t"
                          " "
                        ) ;string-replace
      ) ;string-trim-both
    ) ;define
    (define (%row state session selected? width now-posix)
      (catch #t
        (lambda ()
          (let* ((raw-label (or (pi-session-info-name session) (pi-session-info-first-message session))
                 ) ;raw-label
                 (label
                   (%one-line (utf8-replace-invalid (if (string? raw-label) raw-label "")))
                 ) ;label
                 (right
                   (utf8-replace-invalid
                     (string-join
                       (append (if (eq? (state-scope state) 'all) (list (pi-session-info-cwd session)) '())
                         (if (state-show-path? state) (list (pi-session-info-path session)) '())
                         (list (number->string (pi-session-info-message-count session))
                           (session-selector-format-age (pi-session-info-modified session) now-posix)
                         ) ;list
                       ) ;append
                       " "
                     ) ;string-join
                   ) ;utf8-replace-invalid
                 ) ;right
                 (cursor (if selected? "› " "  "))
                 (available (max 10 (- width (display-width right) 4)))
                 (left (string-append cursor (truncate-to-width label available "…")))
                 (spaces
                   (make-string (max 1 (- width (display-width left) (display-width right)))
                     #\space
                   ) ;make-string
                 ) ;spaces
                 (line (string-append left spaces right))
                ) ;
            (truncate-to-width (if selected? (%colored %accent line) line) width)
          ) ;let*
        ) ;lambda
        (lambda _ (%colored %muted "  (invalid session)"))
      ) ;catch
    ) ;define
    (define (%rows state width)
      (let* ((items (state-visible state))
             (count (length items))
             (selected (state-selected state))
             (start (max 0 (min (- selected 5) (- count 10))))
             (end (min count (+ start 10)))
             ;; 一次渲染内所有行共用同一 now，避免逐行重复读取系统时间
             (now-posix (timestamp->utc-posix-time (current-utc-timestamp)))
            ) ;
        (if (zero? count)
          (list (%colored %muted "  No sessions found"))
          (append
            (map (lambda (s i) (%row state s (= i selected) width now-posix))
              (list-tail items start)
              (iota (- end start) start)
            ) ;map
            (if (or (> start 0) (< end count))
              (list
                (%colored %muted
                  (string-append "  ("
                    (number->string (+ selected 1))
                    "/"
                    (number->string count)
                    ")"
                  ) ;string-append
                ) ;%colored
              ) ;list
              '()
            ) ;if
          ) ;append
        ) ;if
      ) ;let*
    ) ;define
    (define (%render-body state w)
      (let* ((scope (if (eq? (state-scope state) 'current) "Current Folder" "All"))
             (sort (if (eq? (state-sort-mode state) 'recent) "Recent" "Fuzzy"))
             (named (if (eq? (state-name-filter state) 'all) "All" "Named"))
             (border (%colored %border (utf8-make-string w #\─)))
             (search-lines (component-render (state-search state) (max 1 (- w 2))))
             (err (state-error-msg state))
             (err-lines
               (if (string? err)
                 (list (%colored %accent (string-append "  Error: " (utf8-replace-invalid err))))
                 '()
               ) ;if
             ) ;err-lines
            ) ;
        (append
          (list border
            ""
            (string-append "Resume Session (" scope ")  Name: " named "  Sort: " sort)
            "tab scope · fuzzy search"
            (string-append "ctrl+s sort · ctrl+n named · ctrl+p path ("
              (if (state-show-path? state) "on" "off")
              ")"
            ) ;string-append
            ""
            (if (pair? search-lines) (string-append "> " (car search-lines)) "> ")
            ""
          ) ;list
          err-lines
          (%rows state w)
          (list "" border)
        ) ;append
      ) ;let*
    ) ;define

    (define (%render comp width)
      (let ((state (%state comp)) (w (max 1 width)))
        (catch #t
          (lambda () (%render-body state w))
          (lambda (tag . info)
            (list (string-append "Resume Session Error: " (%format-error tag info)))
          ) ;lambda
        ) ;catch
      ) ;let
    ) ;define

    (define (%move! state delta)
      (let ((last (max 0 (- (length (state-visible state)) 1))))
        (state-selected-set! state (max 0 (min last (+ (state-selected state) delta))))
      ) ;let
    ) ;define
    (define (%toggle-scope! state)
      (state-scope-set! state (if (eq? (state-scope state) 'current) 'all 'current))
      (%refresh! state)
    ) ;define
    (define (%dispatch-key state key)
      (cond ((eq? key 'tab) (%toggle-scope! state))
            ((eq? key 'ctrl-n)
             (state-name-filter-set! state
               (if (eq? (state-name-filter state) 'all) 'named 'all)
             ) ;state-name-filter-set!
             (%refresh! state)
            ) ;
            ((eq? key 'ctrl-p) (state-show-path?-set! state (not (state-show-path? state))))
            ((eq? key 'ctrl-s)
             (state-sort-mode-set! state
               (if (eq? (state-sort-mode state) 'recent) 'relevance 'recent)
             ) ;state-sort-mode-set!
             (%refresh! state)
            ) ;
            ((eq? key 'up) (%move! state -1))
            ((eq? key 'down) (%move! state 1))
            ((eq? key 'page-up) (%move! state -10))
            ((eq? key 'page-down) (%move! state 10))
            ((eq? key 'enter)
             (let ((items (state-visible state)))
               (when (pair? items)
                 ((state-on-select state)
                  (pi-session-info-path (list-ref items (state-selected state)))
                 ) ;
               ) ;when
             ) ;let
            ) ;
            ((memq key '(escape ctrl-c)) ((state-on-cancel state)))
            (else (component-handle-input (state-search state) key))
      ) ;cond
    ) ;define

    (define (%handle-input comp key)
      (let ((state (%state comp)))
        (catch #t
          (lambda () (%dispatch-key state key))
          (lambda (tag . info) (state-error-msg-set! state (%format-error tag info)))
        ) ;catch
        #t
      ) ;let
    ) ;define

    (define (session-selector-step-index! comp)
      (let ((state (%state comp)))
        (if (not (and (state? state) (state-incremental? state)))
          #f
          (catch #t
            (lambda ()
              (let* ((res (session-selector-warm-batch! (state-text-cache state) (%source state)))
                     (warmed (car res))
                     (pending? (cdr res))
                     (has-query?
                       (not
                         (string-null? (string-trim-both (editor-text (state-search state))))
                       ) ;not
                     ) ;has-query?
                    ) ;
                (cond ((and (> warmed 0) has-query?) (%refresh! state) 'updated)
                      (pending? 'more)
                      (else #f)
                ) ;cond
              ) ;let*
            ) ;lambda
            (lambda _ #f)
          ) ;catch
        ) ;if
      ) ;let
    ) ;define

    ;; 默认 recent（最后活动时间降序）；当前无 parent 概念，故不采用上游 threaded 默认
    (define* (make-session-selector current-sessions
               all-sessions
               on-select
               on-cancel
               (incremental #f)
             ) ;make-session-selector
      (let* ((state (%make-state current-sessions
                      all-sessions
                      on-select
                      on-cancel
                      'current
                      'recent
                      'all
                      #f
                      #f
                      '()
                      0
                      (make-session-search-cache)
                      #f
                      incremental
                    ) ;%make-state
             ) ;state
             (search (make-text-input ""
                       :multiline #f
                       :focused   #t
                       :on-change (lambda (ed text) (%refresh! state))
                     ) ;make-text-input
             ) ;search
             (comp (make-component :render %render :handle-input %handle-input))
            ) ;
        (state-search-set! state search)
        (component-data-set! comp (list 'session-selector state))
        (%refresh! state)
        comp
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
