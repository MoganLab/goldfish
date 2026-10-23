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

(define-library (liii goldrepl loop)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii list)
    (liii string)
    (liii tui)
    (liii goldrepl style)
    (liii goldrepl complete)
    (liii goldrepl history)
    (liii goldrepl eval)
    (liii goldrepl meta)
    (liii goldrepl render)
  ) ;import
  (export %repl-eval-and-show! %repl-try-complete-first!
    %repl-try-complete-cycle! %repl-submit-meta! %repl-submit-code!
    %repl-preload-mode! %repl-cleanup! %repl-format-completion-menu
  ) ;export
  (begin

    (define (%repl-eval-and-show! code app-out!)
      (let* ((res (%repl-eval code))
             (out (car res))
             (err (cadr res))
             (ok? (caddr res))
             (result (cadddr res))
            ) ;
        (%repl-show-out out app-out!)
        (%repl-show-err err app-out!)
        (%repl-show-val ok? result app-out!)
        (%repl-update-symbol-cache!)
      ) ;let*
    ) ;define

    (define (%repl-format-completion-cand i cand)
      (let* ((name (car cand))
             (raw-doc (cdr cand))
             (doc (if (> (string-length raw-doc) 60)
                    (string-append (substring raw-doc 0 60) "...")
                    raw-doc
                  ) ;if
             ) ;doc
            ) ;
        (string-append " "
          (number->string (+ i 1))
          " "
          (%repl-styled "symbol" name)
          (if (string-null? doc) "" (string-append "  " (%repl-styled "dim" doc)))
        ) ;string-append
      ) ;let*
    ) ;define

    (define (%repl-format-completion-menu cands)
      (let* ((top9 (take cands (min 9 (length cands))))
             (menu-lines
               (let loop
                 ((rest top9) (i 0) (acc '()))
                 (if (null? rest)
                   (reverse acc)
                   (loop (cdr rest) (+ i 1) (cons (%repl-format-completion-cand i (car rest)) acc))
                 ) ;if
               ) ;let
             ) ;menu-lines
             (more (- (length cands) (length top9)))
            ) ;
        (if (> more 0)
          (append menu-lines
            (list (%repl-styled "dim"
                    (string-append " (and " (number->string more) " more completions)")
                  ) ;%repl-styled
            ) ;list
          ) ;append
          menu-lines
        ) ;if
      ) ;let*
    ) ;define

    (define (%repl-try-complete-first! ed text cur word cands app-out!)
      (cond ((null? cands) (values '() 0 #f))
            ((null? (cdr cands))
             (editor-set-text! ed (%repl-replace-word text (cadr cur) word (caar cands)))
             (editor-set-cursor! ed
               0
               (+ (- (string-length text) (string-length word)) (string-length (caar cands)))
             ) ;editor-set-cursor!
             (values '() 0 #t)
            ) ;
            (else
              (let ((common (%repl-common-prefix (map car cands))))
                (when (> (string-length common) (string-length word))
                  (editor-set-text! ed (%repl-replace-word text (cadr cur) word common))
                  (editor-set-cursor! ed
                    0
                    (+ (- (string-length text) (string-length word)) (string-length common))
                  ) ;editor-set-cursor!
                ) ;when
                (app-out! (%repl-format-completion-menu cands))
                (values cands 0 #t)
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (%repl-try-complete-cycle! ed text cur word cands idx)
      (let ((cand (list-ref cands idx)))
        (editor-set-text! ed (%repl-replace-word text (cadr cur) word (car cand)))
        (editor-set-cursor! ed
          0
          (+ (- (string-length text) (string-length word)) (string-length (car cand)))
        ) ;editor-set-cursor!
        (values cands (modulo (+ idx 1) (length cands)) #t)
      ) ;let
    ) ;define

    (define (%repl-submit-meta! ed text hist app-out! eval-show!)
      (let-values (((q lines) (%repl-handle-meta text)))
        (app-out! (list (string-append (%repl-prompt #t) (%repl-highlight-line text))))
        (app-out! lines)
        (when (eq? q 'import)
          (eval-show! (car lines))
        ) ;when
        (editor-set-text! ed "")
        (history-push! hist text)
        (values q (list text))
      ) ;let-values
    ) ;define

    (define (%repl-submit-code! ed text buf hist app-out! eval-show! atom?)
      (let* ((pending (car buf))
             (new-pending (if (pair? pending) (append pending (list text)) (list text)))
             (joined (string-join new-pending "\n"))
             (missing (%repl-missing-parens joined))
             (first? (or (not (pair? pending)) (null? pending)))
            ) ;
        (app-out! (list (string-append (%repl-prompt first?) (%repl-highlight-line text)))
        ) ;app-out!
        (if (and (> missing 0) (not (atom? joined)))
          (begin
            (set-car! buf new-pending)
            (editor-set-text! ed "")
            (values #f '())
          ) ;begin
          (begin
            (eval-show! joined)
            (set-car! buf "")
            (editor-set-text! ed "")
            (history-push! hist joined)
            (values #f (list joined))
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    (define (%repl-preload-mode! mode)
      (when (or (string=? mode "liii") (string=? mode "default"))
        (catch #t
          (lambda ()
            (eval '(import (scheme base) (liii base) (liii error) (liii string)) (rootlet))
          ) ;lambda
          (lambda (tag info) #f)
        ) ;catch
      ) ;when
    ) ;define

    (define (%repl-cleanup! term screen hist-entries old-count)
      (%repl-save-history! (reverse hist-entries) old-count)
      (let ((total (length (screen-lines screen))))
        (when (> total 0)
          (terminal-set-cursor! term 0 total)
        ) ;when
      ) ;let
      (terminal-show-cursor! term)
      (terminal-exit-raw-mode! term)
      (terminal-write term "\r\n")
      (terminal-flush term)
    ) ;define

  ) ;begin
) ;define-library
