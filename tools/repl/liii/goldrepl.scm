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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii goldrepl)
  (import (scheme base)
    (scheme write)
    (scheme process-context)
    (liii base)
    (liii error)
    (liii list)
    (liii os)
    (liii path)
    (liii string)
    (liii time)
    (liii tui)
    (liii tui component)
    (liii goldrepl style)
    (liii goldrepl complete)
    (liii goldrepl history)
    (liii goldrepl eval)
    (liii goldrepl meta)
    (liii goldrepl render)
    (liii goldrepl loop)
  ) ;import
  (export
    ;; style
    *repl-styles*
    %repl-style
    %repl-styled
    %repl-gray
    *repl-keywords*
    %repl-highlight-line
    ;; complete
    *repl-symbols*
    %repl-symbol-char?
    %repl-update-symbol-cache!
    %repl-symbol-name?
    %repl-completions
    %repl-word-at
    %repl-replace-word
    ;; history
    %repl-history-dir
    %repl-history-file
    %repl-escape-history-entry
    %repl-unescape-history-entry
    %repl-history-file-lines
    %repl-load-history!
    %repl-save-history!
    ;; eval & meta
    %repl-missing-parens
    *repl-eval-error*
    %repl-eval
    *repl-value-count*
    %repl-register-value!
    *repl-meta-commands*
    %repl-meta-help
    %repl-meta-apropos
    %repl-meta-describe
    %repl-handle-meta
    ;; buffer & main
    %repl-buffer
    %repl-buffer-set!
    %repl-show-out
    %repl-show-err
    %repl-show-val
    %repl-try-complete-first!
    %repl-format-completion-menu
    %repl-editor-help-lines
    %repl-refresh!
    %repl-mode
    %repl-atom?
    main
  ) ;export
  (begin

    (define (%repl-buffer)
      (list "")
    ) ;define

    (define (%repl-buffer-set! buf lines)
      (set-car! buf lines)
    ) ;define

    (define (main)
      (let* ((term (make-standard-terminal :inline #t))
             (screen (make-main-screen term :auto-hide-cursor #f))
             (ed (make-text-input))
             (hist (editor-history ed))
             (mode (%repl-mode))
             (hist-res (%repl-load-history! hist))
             (old-count (car hist-res))
             (new-entries '())
             (buf (%repl-buffer))
             (scroll '())
             (quit? #f)
             (comp-cands '())
             (comp-idx 0)
            ) ;
        (%repl-preload-mode! mode)
        (%repl-update-symbol-cache!)
        (set! scroll (%repl-banner-lines mode))
        (define (width)
          (max 20 (car (terminal-size term)))
        ) ;define
        (define (refresh!)
          (let ((menu
                  (if (pair? comp-cands)
                    (%repl-format-completion-menu comp-cands (max 20 (- (width) 4)))
                    '()
                  ) ;if
                ) ;menu
               ) ;
            (%repl-refresh! screen term scroll ed (width) (pair? (car buf)) menu)
          ) ;let
        ) ;define
        (define (app-out! lines)
          (when (pair? lines)
            (set! scroll (append scroll lines))
          ) ;when
        ) ;define
        (define (eval-show! c)
          (%repl-eval-and-show! c app-out!)
        ) ;define
        (define (reset-comp!)
          (set! comp-cands '())
          (set! comp-idx 0)
        ) ;define
        (define (do-complete!)
          (let* ((txt (editor-text ed))
                 (cur (editor-cursor ed))
                 (w (%repl-word-at txt (cadr cur)))
                ) ;
            (if (null? comp-cands)
              (let-values (((cands idx ok?)
                            (%repl-try-complete-first! ed txt cur w (%repl-completions w) app-out!)
                           ) ;
                          ) ;
                (set! comp-cands cands)
                (set! comp-idx idx)
              ) ;let-values
              (let-values (((cands idx ok?) (%repl-try-complete-cycle! ed txt cur w comp-cands comp-idx)))
                (set! comp-cands cands)
                (set! comp-idx idx)
              ) ;let-values
            ) ;if
          ) ;let*
        ) ;define
        (define (do-submit!)
          (let ((txt (editor-text ed)) (pending (car buf)))
            (reset-comp!)
            (cond
             ((and (or (not (pair? pending)) (null? pending)) (string-null? txt))
              (app-out! (list (%repl-prompt #t)))
             ) ;
             ((and (or (not (pair? pending)) (null? pending)) (string-starts? txt ","))
              (let-values (((q ents) (%repl-submit-meta! ed txt hist app-out! eval-show!)))
                (when (eq? q #t)
                  (set! quit? #t)
                ) ;when
                (set! new-entries (append ents new-entries))
              ) ;let-values
             ) ;
             (else
               (let-values (((q ents) (%repl-submit-code! ed txt buf hist app-out! eval-show! %repl-atom?))
                           ) ;
                 (set! new-entries (append ents new-entries))
               ) ;let-values
             ) ;else
            ) ;cond
          ) ;let
        ) ;define
        (define (do-newline!)
          (reset-comp!)
          (let* ((txt (editor-text ed))
                 (pending (car buf))
                 (new-p (if (pair? pending) (append pending (list txt)) (list txt)))
                 (first? (or (not (pair? pending)) (null? pending)))
                ) ;
            (app-out! (list (string-append (%repl-prompt first?) (%repl-highlight-line txt)))
            ) ;app-out!
            (%repl-buffer-set! buf new-p)
            (editor-set-text! ed "")
          ) ;let*
        ) ;define
        (define (do-digit-cand! k)
          (let* ((idx (- (char->integer (string-ref k 0)) (char->integer #\1)))
                 (cand (list-ref comp-cands idx))
                 (txt (editor-text ed))
                 (cur (editor-cursor ed))
                 (w (%repl-word-at txt (cadr cur)))
                 (col (cadr cur))
                 (start (- col (string-length w)))
                 (repl (car cand))
                ) ;
            (editor-set-text! ed (%repl-replace-word txt col w repl))
            (editor-set-cursor! ed 0 (+ start (string-length repl)))
            (reset-comp!)
          ) ;let*
        ) ;define
        (define (handle-key! k)
          (cond
           ((and (pair? comp-cands)
              (string? k)
              (= (string-length k) 1)
              (let ((c (string-ref k 0)))
                (and (char>=? c #\1) (char<=? c #\9))
              ) ;let
              (< (- (char->integer (string-ref k 0)) (char->integer #\1)) (length comp-cands))
            ) ;and
            (do-digit-cand! k)
           ) ;
           ((eq? k 'enter) (do-submit!))
           ((eq? k 'f1) (app-out! (%repl-editor-help-lines)) (reset-comp!))
           ((eq? k 'ctrl-l) (set! scroll '()) (reset-comp!))
           ((eq? k 'ctrl-c)
            (if (pair? (car buf))
              (begin
                (app-out! (list (string-append (%repl-prompt #f) (editor-text ed) " ^C")))
                (%repl-buffer-set! buf "")
                (editor-set-text! ed "")
              ) ;begin
              (if (string-null? (editor-text ed)) (set! quit? #t) (editor-set-text! ed ""))
            ) ;if
            (reset-comp!)
           ) ;
           ((eq? k 'ctrl-d)
            (if
              (and (string-null? (editor-text ed)) (not (pair? (car buf))))
              (set! quit? #t)
              (begin
                (reset-comp!)
                (component-handle-input ed 'ctrl-d)
              ) ;begin
            ) ;if
           ) ;
           ((eq? k 'escape) (if (pair? comp-cands) (reset-comp!) (editor-set-text! ed "")))
           ((eq? k 'tab) (do-complete!))
           ((or (eq? k 'shift-tab) (eq? k 'ctrl-enter) (eq? k 'shift-enter) (eq? k 'ctrl-j))
            (do-newline!)
           ) ;
           ((not k) (sleep 0.01))
           (else (reset-comp!) (component-handle-input ed k))
          ) ;cond
        ) ;define
        (dynamic-wind (lambda () (terminal-enter-raw-mode! term) (refresh!))
          (lambda ()
            (let loop
              ()
              (unless quit?
                (handle-key! (terminal-poll-key term))
                (refresh!)
                (loop)
              ) ;unless
            ) ;let
          ) ;lambda
          (lambda () (%repl-cleanup! term screen new-entries old-count))
        ) ;dynamic-wind
        0
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
