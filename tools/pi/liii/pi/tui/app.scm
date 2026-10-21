;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi tui app)
  (export make-pi-app pi-app? pi-app-screen pi-app-editor pi-app-render!
    pi-app-run
  ) ;export
  (import (scheme base)
    (liii time)
    (liii either)
    (liii string)
    (liii subprocess)
    (liii tui)
    (liii pi tui common)
    (liii pi tui header)
    (liii pi tui footer)
  ) ;import
  (begin

    ;; 编辑器顶边框：开启时占 1 行，渲染时光标行需跳过该行
    (define %pi-editor-top-border #t)

    (define-record-type <pi-app>
      (%make-pi-app term screen doc editor footer)
      pi-app?
      (term %pi-app-term)
      (screen pi-app-screen)
      (doc %pi-app-doc)
      (editor pi-app-editor %pi-app-editor-set!)
      (footer %pi-app-footer)
    ) ;define-record-type

    (define (%detect-git-branch)
      (catch #t
        (lambda ()
          (let ((res (run-either '("git" "branch" "--show-current")
                       :stdout  'capture
                       :stderr  'discard
                       :timeout 2
                     ) ;run-either
                ) ;res
               ) ;
            (if (either-right? res)
              (let ((b (string-trim-both (to-right res))))
                (if (string-null? b) #f b)
              ) ;let
              #f
            ) ;if
          ) ;let
        ) ;lambda
        (lambda args #f)
      ) ;catch
    ) ;define

    (define (pi-app-render! app)
      (let* ((screen (pi-app-screen app))
             (term (%pi-app-term app))
             (w (car (terminal-size term)))
             ;; 各段只渲染一次，拼成行列表交给 screen-render!，避免重复渲染与重复取尺寸
             (doc-lines (component-render (%pi-app-doc app) w))
             (ed (pi-app-editor app))
             (ed-lines (component-render ed w))
             (footer-lines (component-render (%pi-app-footer app) w))
             (ed-cur (editor-cursor ed))
             ;; 顶边框占 1 行，光标行紧随其后
             (ed-y (+ (length doc-lines) (if %pi-editor-top-border 1 0)))
             (cur-pos (cons (cadr ed-cur) (+ ed-y (car ed-cur))))
            ) ;
        (screen-render! screen (append doc-lines ed-lines footer-lines) cur-pos)
      ) ;let*
    ) ;define

    (define (make-pi-app . args)
      (let* ((term (%keyword-value :terminal args #f))
             (actual-term (or term (make-standard-terminal :inline #t)))
             (screen (make-main-screen actual-term :auto-hide-cursor #f))
             (header (make-pi-header))
             (chat (make-container))
             (doc (make-container))
             (branch (%detect-git-branch))
             (footer (make-pi-footer :branch branch))
             ;; editor 字段后填：提交回调需要引用 app 本身
             (app (%make-pi-app actual-term screen doc #f footer))
             (ed
               (make-editor ""
                 :multiline     #f
                 :top-border    %pi-editor-top-border
                 :bottom-border #t
                 ;; (liii tui) 的 :border-color 收的是裸 ANSI 前缀（其后由编辑器补复原序列），
                 ;; (liii format color) 只能产出带收尾复原的完整串，故此处保留字面量
                 :border-color "\x1b;[38;2;129;162;190m"
                 :focused      #t
                 :cursor-style 'hardware
                 :on-submit
                 (lambda (self txt)
                   (unless (string-null? (string-trim txt))
                     (container-add! chat (make-text (string-append "> " txt)))
                     (container-add! chat (make-text (string-append "Echo: " txt)))
                   ) ;unless
                   (editor-set-text! self "")
                   (pi-app-render! app)
                 ) ;lambda
               ) ;make-editor
             ) ;ed
            ) ;
        (container-add! doc header)
        (container-add! doc chat)
        (%pi-app-editor-set! app ed)
        app
      ) ;let*
    ) ;define

    (define (pi-app-run app)
      (let* ((term (%pi-app-term app))
             (screen (pi-app-screen app))
             (ed (pi-app-editor app))
            ) ;
        (dynamic-wind (lambda ()
                        (terminal-enter-raw-mode! term)
                        (terminal-hide-cursor! term)
                        (pi-app-render! app)
                      ) ;lambda
          (lambda ()
            (let loop
              ()
              (let ((k (terminal-poll-key term)))
                (cond ((or (eq? k 'ctrl-c) (eq? k 'ctrl-d) (eq? k 'escape)) #t)
                      ((not k) (sleep 0.01) (loop))
                      (else (component-handle-input ed k) (pi-app-render! app) (loop))
                ) ;cond
              ) ;let
            ) ;let
          ) ;lambda
          (lambda ()
            (let* ((lines (screen-lines screen)) (total (length lines)))
              (when (> total 0)
                (terminal-set-cursor! term 0 total)
              ) ;when
              (terminal-show-cursor! term)
              (terminal-exit-raw-mode! term)
              (terminal-write term "\r\n")
              (terminal-flush term)
            ) ;let*
          ) ;lambda
        ) ;dynamic-wind
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
