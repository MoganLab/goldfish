;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui slash-menu types)
  (import (scheme base)
    (scheme char)
    (liii error)
    (liii string)
    (liii pi extension)
  ) ;import
  (export <pi-tui-slash-command> make-pi-tui-slash-command pi-tui-slash-command?
    slash-command-name slash-command-description slash-command-handler
    normalize-slash-command *builtin-slash-commands* collect-slash-commands
    filter-slash-commands <pi-tui-slash-menu> make-pi-tui-slash-menu
    pi-tui-slash-menu? slash-menu-visible? slash-menu-query
    slash-menu-candidates slash-menu-selected-index slash-menu-commands
    %menu-set-visible! %menu-set-query! %menu-set-candidates!
    %menu-set-selected-index! %menu-set-commands!
  ) ;export
  (begin

    (define-record-type <pi-tui-slash-command>
      (%make-pi-tui-slash-command name description handler)
      pi-tui-slash-command?
      (name slash-command-name)
      (description slash-command-description)
      (handler slash-command-handler)
    ) ;define-record-type

    (define <pi-tui-slash-command> '<pi-tui-slash-command>)

    (define (make-pi-tui-slash-command name description . maybe-handler)
      (unless (and (string? name) (not (string=? name "")))
        (type-error "make-pi-tui-slash-command: expected non-empty string name")
      ) ;unless
      (unless (string? description)
        (type-error "make-pi-tui-slash-command: expected string description")
      ) ;unless
      (let ((handler (if (pair? maybe-handler) (car maybe-handler) #f)))
        (when (and handler (not (procedure? handler)))
          (type-error "make-pi-tui-slash-command: expected procedure or #f for handler")
        ) ;when
        (let ((norm-name (if (string-starts? name "/") name (string-append "/" name))))
          (%make-pi-tui-slash-command norm-name description handler)
        ) ;let
      ) ;let
    ) ;define

    (define (normalize-slash-command obj)
      (cond ((pi-tui-slash-command? obj) obj)
            ((extension-command-def? obj)
             (make-pi-tui-slash-command (extension-command-def-name obj)
               (extension-command-def-description obj)
               (extension-command-def-handler obj)
             ) ;make-pi-tui-slash-command
            ) ;
            ((and (pair? obj) (string? (car obj)) (string? (cdr obj)))
             (make-pi-tui-slash-command (car obj) (cdr obj))
            ) ;
            (else (type-error "normalize-slash-command: expected slash-command, extension-command-def, or pair"
                  ) ;type-error
            ) ;else
      ) ;cond
    ) ;define

    ;; 默认兜底内置命令
    (define *builtin-slash-commands*
      (list (make-pi-tui-slash-command "/help" "显示帮助与可用命令")
        (make-pi-tui-slash-command "/exit" "退出当前会话")
        (make-pi-tui-slash-command "/clear" "清空当前屏幕与输入")
      ) ;list
    ) ;define

    ;; 从扩展注册表收集命令并与内置命令去重合并
    (define (collect-slash-commands . maybe-registry)
      (if (or (null? maybe-registry) (not (car maybe-registry)))
        *builtin-slash-commands*
        (let ((target (car maybe-registry)))
          (let ((ext-cmds (catch #t
                            (lambda () (extension-registry-commands-list target))
                            (lambda _ (type-error "collect-slash-commands: invalid extension registry"))
                          ) ;catch
                ) ;ext-cmds
               ) ;
            (unless (list? ext-cmds)
              (type-error "collect-slash-commands: expected extension-registry")
            ) ;unless
            (let* ((normalized-ext (map normalize-slash-command ext-cmds))
                   (all-candidates (append normalized-ext *builtin-slash-commands*))
                  ) ;
              (let loop
                ((rest all-candidates) (seen '()) (acc '()))
                (if (null? rest)
                  (reverse acc)
                  (let* ((cmd (car rest)) (nm (slash-command-name cmd)))
                    (if (member nm seen)
                      (loop (cdr rest) seen acc)
                      (loop (cdr rest) (cons nm seen) (cons cmd acc))
                    ) ;if
                  ) ;let*
                ) ;if
              ) ;let
            ) ;let*
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    (define (%strip-slash s)
      (if (string-starts? s "/") (string-remove-prefix s "/") s)
    ) ;define

    (define (filter-slash-commands commands query)
      (unless (list? commands)
        (type-error "filter-slash-commands: expected list of commands")
      ) ;unless
      (unless (string? query)
        (type-error "filter-slash-commands: expected string query")
      ) ;unless
      (if (or (string=? query "") (string=? query "/"))
        commands
        (let* ((q-norm (string-downcase (%strip-slash query)))
               (prefix-matches '())
               (sub-matches '())
              ) ;
          (for-each
            (lambda (cmd)
              (let* ((nm (slash-command-name cmd)) (nm-norm (string-downcase (%strip-slash nm))))
                (cond ((string-starts? nm-norm q-norm)
                       (set! prefix-matches (cons cmd prefix-matches))
                      ) ;
                      ((string-contains nm-norm q-norm) (set! sub-matches (cons cmd sub-matches)))
                ) ;cond
              ) ;let*
            ) ;lambda
            commands
          ) ;for-each
          (append (reverse prefix-matches) (reverse sub-matches))
        ) ;let*
      ) ;if
    ) ;define

    ;; ------------------------------------------------------------------------
    ;; 3. Slash Menu 数据类型定义
    ;; ------------------------------------------------------------------------

    (define-record-type <pi-tui-slash-menu>
      (%make-pi-tui-slash-menu visible? query candidates selected-index commands)
      pi-tui-slash-menu?
      (visible? slash-menu-visible? %menu-set-visible!)
      (query slash-menu-query %menu-set-query!)
      (candidates slash-menu-candidates %menu-set-candidates!)
      (selected-index slash-menu-selected-index %menu-set-selected-index!)
      (commands slash-menu-commands %menu-set-commands!)
    ) ;define-record-type

    (define <pi-tui-slash-menu> '<pi-tui-slash-menu>)

    (define (%keyword-val key opts default)
      (let loop
        ((rest opts))
        (cond ((null? rest) default)
              ((and (pair? (cdr rest)) (eq? (car rest) key)) (cadr rest))
              ((null? (cdr rest)) default)
              (else (loop (cddr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (make-pi-tui-slash-menu . opts)
      (let* ((vis (%keyword-val :visible? opts #f))
             (qry (%keyword-val :query opts ""))
             (cmds (%keyword-val :commands opts (collect-slash-commands #f)))
             (idx (%keyword-val :selected-index opts 0))
             (cands (%keyword-val :candidates opts #f))
            ) ;
        (unless (boolean? vis)
          (type-error "make-pi-tui-slash-menu: expected boolean for :visible?")
        ) ;unless
        (unless (string? qry)
          (type-error "make-pi-tui-slash-menu: expected string for :query")
        ) ;unless
        (unless (list? cmds)
          (type-error "make-pi-tui-slash-menu: expected list for :commands")
        ) ;unless
        (unless (and (exact-integer? idx) (>= idx 0))
          (type-error "make-pi-tui-slash-menu: expected non-negative integer for :selected-index"
          ) ;type-error
        ) ;unless
        (when (and cands (not (list? cands)))
          (type-error "make-pi-tui-slash-menu: expected list for :candidates")
        ) ;when
        (let ((actual-cands (if (list? cands) cands (filter-slash-commands cmds qry))))
          (%make-pi-tui-slash-menu vis qry actual-cands idx cmds)
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
