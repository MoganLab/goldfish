;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii pi tui app)
  (export make-pi-app pi-app? pi-app-screen pi-app-editor pi-app-render!
    pi-app-run pi-detect-git-branch
  ) ;export
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii os)
    (liii sys)
    (liii time)
    (liii either)
    (liii string)
    (liii subprocess)
    (liii tui)
    (liii pi tui header)
    (liii pi tui footer)
  ) ;import
  (begin

    (define-record-type <pi-app>
      (%make-pi-app term screen root doc-container chat-container editor footer)
      pi-app?
      (term %pi-app-term)
      (screen %pi-app-screen)
      (root %pi-app-root)
      (doc-container %pi-app-doc)
      (chat-container %pi-app-chat)
      (editor %pi-app-editor)
      (footer %pi-app-footer)
    ) ;define-record-type

    (define (pi-app-screen app)
      (%pi-app-screen app)
    ) ;define
    (define (pi-app-editor app)
      (%pi-app-editor app)
    ) ;define

    (define (%keyword-value key opts default)
      (let loop
        ((rest opts))
        (cond ((null? rest) default)
              ((and (pair? (cdr rest)) (eq? (car rest) key)) (cadr rest))
              ((null? (cdr rest)) default)
              (else (loop (cddr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-detect-git-branch)
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
      (let* ((screen (%pi-app-screen app))
             (root (%pi-app-root app))
             (ed (%pi-app-editor app))
             (ed-cur (editor-cursor ed))
             (term (%pi-app-term app))
             (term-sz (terminal-size term))
             (w (car term-sz))
             (doc-lines (component-render (%pi-app-doc app) w))
             (ed-y (+ (length doc-lines) 1))
             ;; top-border 占 1 行
             (cur-pos (cons (cadr ed-cur) (+ ed-y (car ed-cur))))
            ) ;
        (screen-render! screen root cur-pos)
      ) ;let*
    ) ;define

    (define (make-pi-app . args)
      (let* ((term (%keyword-value :terminal args #f))
             (actual-term (or term (make-standard-terminal :inline #t)))
             (screen (make-main-screen actual-term :auto-hide-cursor #f))
             (header (make-pi-header))
             (chat (make-container))
             (doc (make-container))
             (branch (pi-detect-git-branch))
             (footer (make-pi-footer :branch branch))
             (root (make-container))
             (app-box (list #f))
             (ed
               (make-editor ""
                 :multiline     #f
                 :top-border    #t
                 :bottom-border #t
                 :border-color  "\x1b;[38;2;129;162;190m"
                 :focused       #t
                 :cursor-style  'hardware
                 :on-submit
                 (lambda (self txt)
                   (when (car app-box)
                     (let ((app (car app-box)))
                       (unless (string-null? (string-trim txt))
                         (container-add! chat (make-text (string-append "> " txt)))
                         (container-add! chat (make-text (string-append "Echo: " txt)))
                       ) ;unless
                       (editor-set-text! self "")
                       (pi-app-render! app)
                     ) ;let
                   ) ;when
                 ) ;lambda
               ) ;make-editor
             ) ;ed
            ) ;
        (container-add! doc header)
        (container-add! doc chat)
        (container-add! root doc)
        (container-add! root ed)
        (container-add! root footer)
        (let ((app (%make-pi-app actual-term screen root doc chat ed footer)))
          (set-car! app-box app)
          app
        ) ;let
      ) ;let*
    ) ;define

    (define (pi-app-run app)
      (let* ((term (%pi-app-term app))
             (screen (%pi-app-screen app))
             (ed (%pi-app-editor app))
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
              (display "\r\n")
              (terminal-flush term)
            ) ;let*
          ) ;lambda
        ) ;dynamic-wind
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
