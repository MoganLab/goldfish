;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; /name 交互：提交命名任务、查询名称并处理 worker 命名事件
(define-library (liii pi code modes interactive app-name)
  (export pi-app-name-command? pi-app-submit-name! pi-app-handle-name-event!)
  (import (scheme base)
    (liii go)
    (liii string)
    (liii tui)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive views)
    (liii pi code modes interactive footer)
  ) ;import
  (begin

    (define (pi-app-name-command? text)
      (or (string=? text "/name") (string-starts? text "/name "))
    ) ;define

    (define (pi-app-submit-name! app text)
      (let ((name (string-trim-both (substring text 5))) (ed (pi-app-editor app)))
        (editor-set-text! ed "")
        (if (string-null? name)
          (let ((current (pi-app-session-name app)))
            (container-add! (%pi-app-chat app)
              (make-assistant-card (if current (string-append "Session name: " current) "Usage: /name <name>")
              ) ;make-assistant-card
            ) ;container-add!
          ) ;let
          (catch #t
            (lambda () (chan-send! (pi-app-jobs-chan app) (list 'session_name name) 1000))
            (lambda (tag . args)
              (container-add! (%pi-app-chat app)
                (make-tool-card "name" '() 'error "Failed to set session name")
              ) ;container-add!
            ) ;lambda
          ) ;catch
        ) ;if
        (pi-app-render! app)
      ) ;let
    ) ;define

    (define (pi-app-handle-name-event! app ev)
      (case (car ev)
            ((session_name_changed)
             (let ((name (cadr ev)))
               (%pi-app-session-name-set! app name)
               (pi-footer-set-session-name! (%pi-app-footer app) name)
               (container-add! (%pi-app-chat app)
                 (make-assistant-card (string-append "Session name set: " name))
               ) ;container-add!
             ) ;let
            ) ;
            ((session_name_error)
             (container-add! (%pi-app-chat app) (make-tool-card "name" '() 'error (cadr ev)))
            ) ;
            (else #f)
      ) ;case
    ) ;define

  ) ;begin
) ;define-library
