;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; /thinking 命令：直接设置、选择器交互、取消与默认级别保存
(define-library (liii pi code modes interactive app-thinking)
  (export pi-app-thinking-command? pi-app-submit-thinking!
    pi-app-close-selector! pi-app-cycle-thinking!
  ) ;export
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii tui)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive views)
    (liii pi code modes interactive footer)
    (liii pi code modes interactive thinking)
    (liii pi code modes interactive thinking-selector)
  ) ;import
  (begin

    (define (pi-app-thinking-command? text)
      (or (string=? text "/thinking") (string-starts? text "/thinking "))
    ) ;define

    (define (pi-app-close-selector! app)
      (%pi-app-active-selector-set! app #f)
      (editor-set-focused! (pi-app-editor app) #t)
      (pi-app-render! app)
    ) ;define

    (define (%apply-thinking-level! app level)
      (%pi-app-thinking-level-set! app level)
      (pi-footer-set-thinking-level! (%pi-app-footer app) level)
      (editor-set-border-color! (pi-app-editor app) (pi-thinking-border-color level))
    ) ;define

    (define (pi-app-cycle-thinking! app)
      (if (pi-app-supports-thinking? app)
        (let ((next (pi-next-thinking-level (pi-app-thinking-level app)
                      (pi-app-available-thinking-levels app)
                    ) ;pi-next-thinking-level
              ) ;next
             ) ;
          (%apply-thinking-level! app next)
          next
        ) ;let
        #f
      ) ;if
    ) ;define

    (define (%select-thinking-level! app level persist?)
      (catch #t
        (lambda ()
          (%apply-thinking-level! app level)
          (when persist?
            (let ((save (%pi-app-on-save-thinking-level app)))
              (when (procedure? save)
                (save level)
              ) ;when
            ) ;let
            (%pi-app-default-thinking-level-set! app level)
          ) ;when
          (%pi-app-active-selector-set! app #f)
          (editor-set-focused! (pi-app-editor app) #t)
          (container-add! (%pi-app-chat app)
            (make-assistant-card (string-append (if persist? "Default thinking level: " "Thinking level: ")
                                   level
                                 ) ;string-append
            ) ;make-assistant-card
          ) ;container-add!
          (pi-app-render! app)
        ) ;lambda
        (lambda (tag . args)
          (%pi-app-active-selector-set! app #f)
          (editor-set-focused! (pi-app-editor app) #t)
          (container-add! (%pi-app-chat app)
            (make-tool-card "thinking" '() 'error "Failed to set thinking level")
          ) ;container-add!
          (pi-app-render! app)
        ) ;lambda
      ) ;catch
    ) ;define

    (define (%show-thinking-selector! app)
      (editor-set-focused! (pi-app-editor app) #f)
      (%pi-app-active-selector-set! app
        (make-thinking-selector (pi-app-thinking-level app)
          (pi-app-available-thinking-levels app)
          (pi-app-default-thinking-level app)
          (lambda (level) (%select-thinking-level! app level #f))
          (lambda () (pi-app-close-selector! app))
          (lambda (level) (%select-thinking-level! app level #t))
        ) ;make-thinking-selector
      ) ;%pi-app-active-selector-set!
      (pi-app-render! app)
    ) ;define

    (define (pi-app-submit-thinking! app text)
      (editor-set-text! (pi-app-editor app) "")
      (let* ((query (string-trim-both (substring text 9)))
             (available (pi-app-available-thinking-levels app))
            ) ;
        (if (string-null? query)
          (%show-thinking-selector! app)
          (let* ((normalized (string-downcase query))
                 (level
                   (find (lambda (candidate) (string=? (string-downcase candidate) normalized))
                     available
                   ) ;find
                 ) ;level
                ) ;
            (if level
              (%select-thinking-level! app level #f)
              (begin
                (container-add! (%pi-app-chat app)
                  (make-tool-card "thinking"
                    '()
                    'error
                    (string-append "Unknown thinking level \""
                      query
                      "\". Available levels: "
                      (string-join available ", ")
                      "."
                    ) ;string-append
                  ) ;make-tool-card
                ) ;container-add!
                (pi-app-render! app)
              ) ;begin
            ) ;if
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
