;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; Thinking Level 选择器：搜索、级别列表、Enter 选择与 Ctrl+S 保存默认值
(define-library (liii pi code modes interactive thinking-selector)
  (export make-thinking-selector thinking-selector? thinking-selector-list
    thinking-selector-search thinking-selector-cursor-position
  ) ;export
  (import (scheme base)
    (liii string)
    (liii list)
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

    (define (%level-description level)
      (cond ((string=? level "off") "No reasoning")
            ((string=? level "minimal") "Very brief reasoning (~1k tokens)")
            ((string=? level "low") "Light reasoning (~2k tokens)")
            ((string=? level "medium") "Moderate reasoning (~8k tokens)")
            ((string=? level "high") "Deep reasoning (~16k tokens)")
            ((string=? level "xhigh") "Extra-high reasoning (~32k tokens)")
            ((string=? level "max") "Maximum reasoning")
            (else "")
      ) ;cond
    ) ;define

    (define-record-type <thinking-selector-state>
      (%make-thinking-selector-state current available default on-select
        on-cancel on-save search list
      ) ;%make-thinking-selector-state
      thinking-selector-state?
      (current thinking-selector-state-current)
      (available thinking-selector-state-available)
      (default thinking-selector-state-default)
      (on-select thinking-selector-state-on-select)
      (on-cancel thinking-selector-state-on-cancel)
      (on-save thinking-selector-state-on-save)
      (search thinking-selector-state-search thinking-selector-state-search-set!)
      (list thinking-selector-state-list thinking-selector-state-list-set!)
    ) ;define-record-type

    (define (%state comp)
      (let ((data (component-data comp)))
        (and (pair? data) (eq? (car data) 'thinking-selector) (cadr data))
      ) ;let
    ) ;define

    (define (thinking-selector? obj)
      (and (component? obj) (thinking-selector-state? (%state obj)))
    ) ;define

    (define (thinking-selector-list comp)
      (thinking-selector-state-list (%state comp))
    ) ;define

    (define (thinking-selector-search comp)
      (thinking-selector-state-search (%state comp))
    ) ;define

    (define (thinking-selector-cursor-position comp)
      (let ((search (thinking-selector-search comp)))
        (list 6 (+ 2 (editor-display-col search)))
      ) ;let
    ) ;define

    (define (%level-items state)
      (map
        (lambda (level)
          (let ((description (%level-description level)))
            (list (cons 'value level)
              (cons 'label
                (string-append (if (string=? level (thinking-selector-state-current state)) "✓ " "  ")
                  level
                ) ;string-append
              ) ;cons
              (cons 'description
                (if (string=? level (thinking-selector-state-default state))
                  (string-append description " · default")
                  description
                ) ;if
              ) ;cons
            ) ;list
          ) ;let
        ) ;lambda
        (thinking-selector-state-available state)
      ) ;map
    ) ;define

    (define (%build-list state items preselect)
      (let ((lst
              (make-select-list items
                (max 1 (length items))
                :min-primary-column-width 12
                :max-primary-column-width 32
                :selected-text    (lambda (text) (%colored %accent text))
                :description-text (lambda (text) (%colored %muted text))
                :scroll-text      (lambda (text) (%colored %muted text))
                :no-match-text    (lambda (text) (%colored %muted text))
                :on-select
                (lambda (item)
                 ((thinking-selector-state-on-select state) (cdr (assoc 'value item)))
                ) ;lambda
                :on-cancel
                (lambda () ((thinking-selector-state-on-cancel state)))
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
      (let* ((old-list (thinking-selector-state-list state))
             (selected (and old-list (select-list-selected-item old-list)))
             (preselect (and selected (cdr (assoc 'value selected))))
             (query (editor-text (thinking-selector-state-search state)))
             (filtered
               (fuzzy-filter (%level-items state)
                 query
                 (lambda (item)
                   (string-append (cdr (assoc 'value item)) " " (cdr (assoc 'description item)))
                 ) ;lambda
               ) ;fuzzy-filter
             ) ;filtered
            ) ;
        (thinking-selector-state-list-set! state (%build-list state filtered preselect))
      ) ;let*
    ) ;define

    (define (%render comp width)
      (let* ((state (%state comp))
             (border (%colored %border (utf8-make-string (max 1 width) #\─)))
             (search (thinking-selector-state-search state))
             (search-lines (component-render search (max 1 (- width 2))))
             (search-line (if (pair? search-lines) (string-append "> " (car search-lines)) "> ")
             ) ;search-line
             (hint (%colored %muted
                     "  enter to select · ctrl+s to set as default · esc to cancel"
                   ) ;%colored
             ) ;hint
            ) ;
        (append (list border "" "Thinking Level" ""
                  "shift+tab cycles thinking levels in-session" "" search-line
                  ""
                ) ;list
          (component-render (thinking-selector-state-list state) width)
          (list "" hint border)
        ) ;append
      ) ;let*
    ) ;define

    (define (%handle-input comp key)
      (let ((state (%state comp)))
        (cond
         ((eq? key 'ctrl-s)
          (let ((item (select-list-selected-item (thinking-selector-state-list state))))
            (when item
              ((thinking-selector-state-on-save state) (cdr (assoc 'value item)))
            ) ;when
          ) ;let
          #t
         ) ;
         ((memq key '(up down enter escape ctrl-c))
          (component-handle-input (thinking-selector-state-list state) key)
         ) ;
         (else (component-handle-input (thinking-selector-state-search state) key))
        ) ;cond
      ) ;let
    ) ;define

    (define (make-thinking-selector current available default on-select on-cancel on-save)
      (let* ((state (%make-thinking-selector-state current available default
                      on-select on-cancel on-save #f #f
                    ) ;%make-thinking-selector-state
             ) ;state
             (search (make-text-input ""
                       :multiline #f
                       :focused   #t
                       :on-change (lambda (ed text) (%refresh-list! state))
                     ) ;make-text-input
             ) ;search
             (comp (make-component :render %render :handle-input %handle-input))
            ) ;
        (thinking-selector-state-search-set! state search)
        (thinking-selector-state-list-set! state
          (%build-list state (%level-items state) current)
        ) ;thinking-selector-state-list-set!
        (component-data-set! comp (list 'thinking-selector state))
        comp
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
