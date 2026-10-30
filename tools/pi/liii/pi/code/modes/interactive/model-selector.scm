;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; Model 选择器：搜索、模型列表（当前模型置顶 ✓）、Enter 选择与 Ctrl+S 保存默认
;; （对齐上游 ModelSelectorComponent，简化 all/scoped scope 与网络目录刷新）
(define-library (liii pi code modes interactive model-selector)
  (export make-model-selector model-selector? model-selector-list
    model-selector-search model-selector-cursor-position
  ) ;export
  (import (scheme base)
    (liii string)
    (liii list)
    (liii sort)
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

    (define (%model-ref model key)
      (cdr (or (assoc key model) (cons #f #f)))
    ) ;define

    (define (%same-model? a b)
      (and (string=? (%model-ref a 'provider) (%model-ref b 'provider))
        (string=? (%model-ref a 'id) (%model-ref b 'id))
      ) ;and
    ) ;define

    ;; 排序键：provider 与 id 用 \x0; 分隔，避免 id 内含 / 造成复合键歧义
    (define (%sort-key model)
      (string-append (%model-ref model 'provider) "\x00;" (%model-ref model 'id))
    ) ;define

    ;; 排序（对齐上游 sortModels）：当前模型最前，默认模型次之，其余按 provider/id
    (define (%sort-models models current default)
      (let* ((sorted
               (list-sort (lambda (a b) (string<? (%sort-key a) (%sort-key b))) models)
             ) ;sorted
             (default-first
               (if default
                 (let ((d (filter (lambda (m) (%same-model? default m)) sorted))
                       (rest
                         (filter (lambda (m) (not (%same-model? default m))) sorted)
                       ) ;rest
                      ) ;
                   (append d rest)
                 ) ;let
                 sorted
               ) ;if
             ) ;default-first
             (current-first
               (if current
                 (let ((c (filter (lambda (m) (%same-model? current m)) default-first))
                       (rest
                         (filter (lambda (m) (not (%same-model? current m))) default-first)
                       ) ;rest
                      ) ;
                   (append c rest)
                 ) ;let
                 default-first
               ) ;if
             ) ;current-first
            ) ;
        current-first
      ) ;let*
    ) ;define

    (define-record-type <model-selector-state>
      (%make-model-selector-state current default models on-select on-cancel
        on-save search list
      ) ;%make-model-selector-state
      model-selector-state?
      (current model-selector-state-current)
      (default model-selector-state-default)
      (models model-selector-state-models)
      (on-select model-selector-state-on-select)
      (on-cancel model-selector-state-on-cancel)
      (on-save model-selector-state-on-save)
      (search model-selector-state-search model-selector-state-search-set!)
      (list model-selector-state-list model-selector-state-list-set!)
    ) ;define-record-type

    (define (%state comp)
      (let ((data (component-data comp)))
        (and (pair? data) (eq? (car data) 'model-selector) (cadr data))
      ) ;let
    ) ;define

    (define (model-selector? obj)
      (and (component? obj) (model-selector-state? (%state obj)))
    ) ;define

    (define (model-selector-list comp)
      (model-selector-state-list (%state comp))
    ) ;define

    (define (model-selector-search comp)
      (model-selector-state-search (%state comp))
    ) ;define

    (define (model-selector-cursor-position comp)
      (let ((search (model-selector-search comp)))
        (list 6 (+ 2 (editor-display-col search)))
      ) ;let
    ) ;define

    (define (%model-items state)
      (let ((current (model-selector-state-current state))
            (default (model-selector-state-default state))
           ) ;
        (map
          (lambda (model)
            (let* ((id (%model-ref model 'id))
                   (name (%model-ref model 'name))
                   (label
                     (string-append (if (and current (%same-model? current model)) "✓ " "  ")
                       id
                       " ["
                       (%model-ref model 'provider)
                       "]"
                     ) ;string-append
                   ) ;label
                   (description (if (and default (%same-model? default model))
                                  (if (string-null? name) "· default" (string-append name " · default"))
                                  name
                                ) ;if
                   ) ;description
                  ) ;
              (list (cons 'value model) (cons 'label label) (cons 'description description))
            ) ;let*
          ) ;lambda
          (model-selector-state-models state)
        ) ;map
      ) ;let
    ) ;define

    ;; 搜索文本（对齐上游 getModelSelectorSearchText）：provider 前缀复合引用优先
    (define (%search-text model)
      (string-append (%model-ref model 'provider)
        " "
        (%model-ref model 'provider)
        "/"
        (%model-ref model 'id)
        " "
        (%model-ref model 'id)
        " "
        (%model-ref model 'name)
      ) ;string-append
    ) ;define

    (define (%build-list state items preselect)
      (let ((lst
              (make-select-list items
                10
                :min-primary-column-width 12
                :max-primary-column-width 48
                :selected-text    (lambda (text) (%colored %accent text))
                :description-text (lambda (text) (%colored %muted text))
                :scroll-text      (lambda (text) (%colored %muted text))
                :no-match-text    (lambda (text) (%colored %muted text))
                :on-select
                (lambda (item)
                 ((model-selector-state-on-select state) (cdr (assoc 'value item)))
                ) ;lambda
                :on-cancel (lambda () ((model-selector-state-on-cancel state)))
              ) ;make-select-list
            ) ;lst
            (index
              (and preselect
                (list-index
                  (lambda (item) (%same-model? preselect (cdr (assoc 'value item))))
                  items
                ) ;list-index
              ) ;and
            ) ;index
           ) ;
        (when index
          (select-list-set-selected-index! lst index)
        ) ;when
        lst
      ) ;let
    ) ;define

    (define (%filtered-items state query)
      (fuzzy-filter (%model-items state)
        query
        (lambda (item) (%search-text (cdr (assoc 'value item))))
      ) ;fuzzy-filter
    ) ;define

    (define (%refresh-list! state)
      (let* ((old-list (model-selector-state-list state))
             (selected (and old-list (select-list-selected-item old-list)))
             (old-value (and selected (cdr (assoc 'value selected))))
             (query (editor-text (model-selector-state-search state)))
             (filtered (%filtered-items state query))
             ;; 对齐上游：有查询时选中置顶（最佳命中），清空查询时保持原选中位置
             (preselect (if (string-null? (string-trim-both query)) old-value #f))
            ) ;
        (model-selector-state-list-set! state (%build-list state filtered preselect))
      ) ;let*
    ) ;define

    (define (%render comp width)
      (let* ((state (%state comp))
             (border (%colored %border (utf8-make-string (max 1 width) #\─)))
             (search (model-selector-state-search state))
             (search-lines (component-render search (max 1 (- width 2))))
             (search-line (if (pair? search-lines) (string-append "> " (car search-lines)) "> ")
             ) ;search-line
             (hint (%colored %muted
                     "  enter to select · ctrl+s to set as default · esc to cancel"
                   ) ;%colored
             ) ;hint
            ) ;
        (append (list border "" "Model" ""
                  "Only showing models from configured providers." ""
                  search-line ""
                ) ;list
          (component-render (model-selector-state-list state) width)
          (list "" hint border)
        ) ;append
      ) ;let*
    ) ;define

    (define (%handle-input comp key)
      (let ((state (%state comp)))
        (cond
         ((eq? key 'ctrl-s)
          (let ((item (select-list-selected-item (model-selector-state-list state))))
            (when item
              ((model-selector-state-on-save state) (cdr (assoc 'value item)))
            ) ;when
          ) ;let
          #t
         ) ;
         ((memq key '(up down enter escape ctrl-c))
          (component-handle-input (model-selector-state-list state) key)
         ) ;
         (else (component-handle-input (model-selector-state-search state) key))
        ) ;cond
      ) ;let
    ) ;define

    (define (make-model-selector current default models on-select on-cancel
             on-save . opt-initial-search
            ) ;
      (let* ((initial-search (if (pair? opt-initial-search) (car opt-initial-search) ""))
             (state (%make-model-selector-state current
                      default
                      (%sort-models (or models '()) current default)
                      on-select
                      on-cancel
                      on-save
                      #f
                      #f
                    ) ;%make-model-selector-state
             ) ;state
             (search (make-text-input initial-search
                       :multiline #f
                       :focused   #t
                       :on-change (lambda (ed text) (%refresh-list! state))
                     ) ;make-text-input
             ) ;search
             (comp (make-component :render %render :handle-input %handle-input))
            ) ;
        (model-selector-state-search-set! state search)
        (model-selector-state-list-set! state
          (%build-list state
            (if (string-null? (string-trim-both initial-search))
              (%model-items state)
              (%filtered-items state initial-search)
            ) ;if
            current
          ) ;%build-list
        ) ;model-selector-state-list-set!
        (component-data-set! comp (list 'model-selector state))
        comp
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
