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
    (liii unicode)
    (liii tui)
    (liii tui component)
    (liii pi code modes interactive session-selector-time)
  ) ;import
  (export make-session-selector session-selector? session-selector-scope
    session-selector-sort-mode session-selector-name-filter
    session-selector-show-path? session-selector-visible-sessions
    session-selector-search session-selector-selected-index
    session-selector-format-age
  ) ;export
  (begin

    (define %accent "\x1b;[35m")
    (define %muted "\x1b;[38;2;157;165;169m")
    (define %border "\x1b;[38;2;91;127;200m")
    (define %reset "\x1b;[0m")
    (define (%colored color text)
      (string-append color text %reset)
    ) ;define
    (define (%ref alist key)
      (cdr (assoc key alist))
    ) ;define

    (define-record-type <session-selector-state>
      (%make-state current all on-select on-cancel scope sort-mode name-filter
        show-path? search visible selected
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
    (define (%search-text session)
      (string-join (list (%ref session 'id)
                     (or (%ref session 'name) "")
                     (%ref session 'all-messages-text)
                     (%ref session 'cwd)
                   ) ;list
        " "
      ) ;string-join
    ) ;define
    (define (%refresh! state)
      (let* ((named
               (if (eq? (state-name-filter state) 'named)
                 (filter (lambda (s) (%ref s 'name)) (%source state))
                 (%source state)
               ) ;if
             ) ;named
             (query (string-trim-both (editor-text (state-search state))))
             (matched (if (string-null? query) named (fuzzy-filter named query %search-text))
             ) ;matched
             (visible
               (if (or (string-null? query) (eq? (state-sort-mode state) 'relevance))
                 matched
                 (filter (lambda (s) (memq s matched)) named)
               ) ;if
             ) ;visible
             (last (max 0 (- (length visible) 1)))
            ) ;
        (state-visible-set! state visible)
        (state-selected-set! state (max 0 (min (state-selected state) last)))
      ) ;let*
    ) ;define

    (define (%one-line text)
      (string-trim-both (string-replace (string-replace (string-replace text "\r" " ") "\n" " ")
                          "\t"
                          " "
                        ) ;string-replace
      ) ;string-trim-both
    ) ;define
    (define (%row state session selected? width)
      (let* ((label (%one-line (or (%ref session 'name) (%ref session 'first-message))))
             (right
               (string-join
                 (append (if (eq? (state-scope state) 'all) (list (%ref session 'cwd)) '())
                   (if (state-show-path? state) (list (%ref session 'path)) '())
                   (list (number->string (%ref session 'message-count))
                     (session-selector-format-age (%ref session 'modified))
                   ) ;list
                 ) ;append
                 " "
               ) ;string-join
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
    ) ;define
    (define (%rows state width)
      (let* ((items (state-visible state))
             (count (length items))
             (selected (state-selected state))
             (start (max 0 (min (- selected 5) (- count 10))))
             (end (min count (+ start 10)))
            ) ;
        (if (zero? count)
          (list (%colored %muted "  No sessions found"))
          (append
            (map (lambda (s i) (%row state s (= i selected) width))
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
    (define (%render comp width)
      (let* ((state (%state comp))
             (w (max 1 width))
             (scope (if (eq? (state-scope state) 'current) "Current Folder" "All"))
             (sort (if (eq? (state-sort-mode state) 'recent) "Recent" "Fuzzy"))
             (named (if (eq? (state-name-filter state) 'all) "All" "Named"))
             (border (%colored %border (utf8-make-string w #\─)))
             (search-lines (component-render (state-search state) (max 1 (- w 2))))
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
          (%rows state w)
          (list "" border)
        ) ;append
      ) ;let*
    ) ;define

    (define (%move! state delta)
      (let ((last (max 0 (- (length (state-visible state)) 1))))
        (state-selected-set! state (max 0 (min last (+ (state-selected state) delta))))
      ) ;let
    ) ;define
    (define (%handle-input comp key)
      (let ((state (%state comp)))
        (cond
         ((eq? key 'tab)
          (state-scope-set! state (if (eq? (state-scope state) 'current) 'all 'current))
          (%refresh! state)
         ) ;
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
              ((state-on-select state) (%ref (list-ref items (state-selected state)) 'path))
            ) ;when
          ) ;let
         ) ;
         ((memq key '(escape ctrl-c)) ((state-on-cancel state)))
         (else (component-handle-input (state-search state) key))
        ) ;cond
        #t
      ) ;let
    ) ;define

    (define (make-session-selector current-sessions all-sessions on-select on-cancel)
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
                    ) ;%make-state
             ) ;state
             (search (make-text-input ""
                       :multiline    #f
                       :focused      #t
                       :cursor-style 'hardware
                       :on-change    (lambda (ed text) (%refresh! state))
                     ) ;make-text-input
             ) ;search
             (comp (make-component :render %render :handle-input %handle-input))
            ) ;
        (state-search-set! state search)
        (component-data-set! comp (list 'session-selector state))
        (%refresh! state)
        comp
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
