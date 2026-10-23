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

(define-library (liii goldrepl render)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii list)
    (liii string)
    (liii tui)
    (liii tui component)
    (liii goldrepl style)
    (liii goldrepl eval)
  ) ;import
  (export %repl-s7-version-text %repl-mode-desc-line %repl-banner-lines
    %repl-prompt %repl-render-input-lines %repl-refresh! %repl-show-out
    %repl-show-err %repl-show-val %repl-editor-help-lines
  ) ;export
  (begin

    (define (%repl-s7-version-text)
      (let ((s7-ver
              (catch #t
                (lambda () (let ((v (*s7* 'version))) (if (string? v) v "")))
                (lambda (t i) "")
              ) ;catch
            ) ;s7-ver
            (s7-date
              (catch #t
                (lambda () (let ((d (*s7* 'date))) (if (string? d) d "")))
                (lambda (t i) "")
              ) ;catch
            ) ;s7-date
           ) ;
        (if (not (string-null? s7-ver))
          (string-append (%repl-styled "italic" (string-append "Based on S7 Scheme " s7-ver))
            (if (not (string-null? s7-date))
              (string-append " " (%repl-styled "dim" (string-append "(" s7-date ")")))
              ""
            ) ;if
          ) ;string-append
          (%repl-styled "italic" "Interactive REPL powered by (liii tui)")
        ) ;if
      ) ;let
    ) ;define

    (define (%repl-mode-desc-line mode)
      (if (or (string=? mode "liii") (string=? mode "default"))
        (string-append (%repl-styled "bold" "Mode: ")
          (%repl-styled "bold" mode)
          " (additionally imports: (scheme base) (liii base) (liii error) (liii string) compared to r7rs)"
        ) ;string-append
        (string-append (%repl-styled "bold" "Mode: ") (%repl-styled "bold" mode))
      ) ;if
    ) ;define

    (define (%repl-banner-lines mode)
      (append
        (list (string-append (%repl-styled "brand" "Goldfish Scheme")
                " "
                (%repl-styled "version" (version))
                " by LiiiLabs"
              ) ;string-append
          (%repl-s7-version-text)
          ""
        ) ;list
        (list (%repl-mode-desc-line mode)
          ""
          "- Type ',quit' or ',q' to quit. (or use ctrl-d)."
          "- Type ',help' for REPL commands help."
          "- Press F1 for help on editing commands."
          "- Use shift-tab for multiline input. (or ctrl-enter, or ctrl-j)"
          "- Use up/down to navigate history, tab to complete."
          ""
        ) ;list
      ) ;append
    ) ;define

    (define (%repl-prompt first?)
      (if first? (%repl-styled "prompt" "gf> ") (%repl-styled "prompt" "... "))
    ) ;define

    (define (%repl-render-input-lines ed width pending?)
      (let* ((text-lines (editor-lines ed))
             (prompt (%repl-prompt (not pending?)))
             (cont (%repl-prompt #f))
            ) ;
        (let loop
          ((rest text-lines) (i 0) (acc '()))
          (if (null? rest)
            (reverse acc)
            (let* ((line (car rest))
                   (hl (%repl-highlight-line line))
                   (prefix (if (= i 0) prompt cont))
                  ) ;
              (loop (cdr rest) (+ i 1) (cons (string-append prefix hl) acc))
            ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (%repl-refresh! screen term scroll-lines ed width pending?)
      (let* ((input-lines (%repl-render-input-lines ed width pending?))
             (all (append scroll-lines input-lines))
             (cursor (editor-cursor ed))
             (cur-pos (cons (+ 4 (editor-display-col ed)) (+ (length scroll-lines) (car cursor)))
             ) ;cur-pos
            ) ;
        (screen-render! screen all cur-pos)
      ) ;let*
    ) ;define

    (define (%repl-show-out out app-out!)
      (when (> (string-length out) 0)
        (let* ((has-newline? (string-ends? out "\n"))
               (disp-out (if has-newline? out (string-append out "↩\n")))
               (ls (string-split disp-out "\n"))
               (clean-ls (if (string-ends? disp-out "\n") (drop-right ls 1) ls))
              ) ;
          (app-out! clean-ls)
        ) ;let*
      ) ;when
    ) ;define

    (define (%repl-show-err err app-out!)
      (when (> (string-length err) 0)
        (app-out! (map (lambda (l) (%repl-styled "error" l)) (string-split err "\n")))
      ) ;when
    ) ;define

    (define (%repl-show-val ok? result app-out!)
      (when ok?
        (let* ((name (symbol->string (%repl-register-value! result)))
               (rs (object->string result))
              ) ;
          (app-out! (list (string-append name " " (%repl-gray "=") " " rs)))
        ) ;let*
      ) ;when
    ) ;define

    (define (%repl-editor-help-lines)
      (list (%repl-styled "bold" "Goldfish Scheme REPL Editing Commands:")
        ""
        (%repl-styled "bold" "Navigation:")
        "  left, ctrl-b     : go one character to the left"
        "  right, ctrl-f    : go one character to the right"
        "  up, ctrl-p       : go one line up, or back in history"
        "  down, ctrl-n     : go one line down, or forward in history"
        "  ctrl-left, alt-b : go to the start of the previous word"
        "  ctrl-right, alt-f: go to the end of the current word"
        "  home, ctrl-a     : go to the start of the current line"
        "  end, ctrl-e      : go to the end of the current line"
        ""
        (%repl-styled "bold" "Deletion:")
        "  del, ctrl-d      : delete the current character (or exit if empty)"
        "  backsp, ctrl-h   : delete the previous character"
        "  ctrl-w, alt-bs   : delete to preceding whitespace or word"
        "  ctrl-u           : delete to the start of the current line"
        "  ctrl-k           : delete to the end of the current line"
        "  esc              : delete the current input"
        ""
        (%repl-styled "bold" "Editing & Multiline:")
        "  enter            : accept current input (or newline if parens unclosed)"
        "  shift-tab        : create a new line for multi-line input"
        "  ctrl-j           : create a new line for multi-line input"
        "  ctrl-l           : clear screen"
        "  ctrl-t           : swap with previous character"
        "  tab              : try to complete the current input"
        ""
      ) ;list
    ) ;define

  ) ;begin
) ;define-library
