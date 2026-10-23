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
    (scheme process-context)
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
  (export %repl-eval-and-show! %repl-apply-completion! %repl-try-complete-first!
    %repl-try-complete-cycle! %repl-submit-meta! %repl-submit-code!
    %repl-preload-mode! %repl-cleanup! %repl-format-completion-menu %repl-mode
    %repl-atom?
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
        (%repl-invalidate-symbol-cache!)
      ) ;let*
    ) ;define

    (define (%repl-format-completion-cand i cand max-w)
      (let* ((name (car cand))
             (raw-doc (cdr cand))
             (prefix-len (+ 5 (string-length name)))
             (avail (max 0 (- max-w prefix-len 3)))
             (doc
               (cond ((<= avail 3) "")
                     ((> (string-length raw-doc) avail)
                      (string-append (substring raw-doc 0 (- avail 3)) "...")
                     ) ;
                     (else raw-doc)
               ) ;cond
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

    (define (%repl-format-completion-menu cands . opt-w)
      (let* ((max-w (if (pair? opt-w) (car opt-w) 70))
             (top9 (take cands (min 9 (length cands))))
             (menu-lines
               (let loop
                 ((rest top9) (i 0) (acc '()))
                 (if (null? rest)
                   (reverse acc)
                   (loop (cdr rest)
                     (+ i 1)
                     (cons (%repl-format-completion-cand i (car rest) max-w) acc)
                   ) ;loop
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

    (define (%repl-apply-completion! ed text cur word repl)
      (let* ((col (cadr cur)) (start (- col (string-length word))))
        (editor-set-text! ed (%repl-replace-word text col word repl))
        (editor-set-cursor! ed 0 (+ start (string-length repl)))
      ) ;let*
    ) ;define

    (define (%repl-try-complete-first! ed text cur word cands)
      (cond ((null? cands) (values '() 0 #f))
            ((null? (cdr cands))
             (%repl-apply-completion! ed text cur word (caar cands))
             (values '() 0 #t)
            ) ;
            (else
              (let ((common (%repl-common-prefix (map car cands))))
                (when (> (string-length common) (string-length word))
                  (%repl-apply-completion! ed text cur word common)
                ) ;when
                (values cands 0 #t)
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (%repl-try-complete-cycle! ed text cur word cands idx)
      (%repl-apply-completion! ed text cur word (car (list-ref cands idx)))
      (values cands (modulo (+ idx 1) (length cands)) #t)
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

    (define (%repl-mode)
      (let* ((args (command-line))
             (m
               (let loop
                 ((rest args))
                 (cond ((null? rest) #f)
                       ((and (pair? (cdr rest)) (member (car rest) '("--mode"
                                                                     "-m"))) (cadr rest))
                       ((and (string? (car rest)) (string-starts? (car rest) "--mode="))
                        (substring (car rest) 7)
                       ) ;
                       ((and (string? (car rest)) (string-starts? (car rest) "-m="))
                        (substring (car rest) 3)
                       ) ;
                       (else (loop (cdr rest)))
                 ) ;cond
               ) ;let
             ) ;m
            ) ;
        (or m "liii")
      ) ;let*
    ) ;define

    (define (%repl-atom? text)
      (and (> (string-length text) 0)
        (not (char=? (string-ref text 0) #\())
        (not (string-contains text "("))
        (not (string-contains text ")"))
      ) ;and
    ) ;define

  ) ;begin
) ;define-library
