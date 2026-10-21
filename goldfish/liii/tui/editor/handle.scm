;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2024-2026 The Goldfish Scheme Authors
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

(define-library (liii tui editor handle)
  (export %editor-render %editor-handle-input)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii unicode)
    (liii tui component)
    (liii tui editor history)
    (liii tui editor buffer)
    (liii tui editor cursor)
    (liii tui editor view)
    (liii tui editor state)
  ) ;import
  (begin

    (define (%make-h-border width)
      (if (<= width 0) "" (utf8-make-string width #\─))
    ) ;define

    (define (%editor-render comp width)
      (let* ((data (component-data comp))
             (buf (%ed-buffer data))
             (lines (buffer-lines buf))
             (row (buffer-row buf))
             (col (buffer-col buf))
             (focused? (%ed-focused? data))
             (ph (%ed-placeholder data))
             (cursor-style (%ed-cursor-style data))
             (top-b (%ed-top-border data))
             (bot-b (%ed-bottom-border data))
             (rendered (render-editor-lines lines row col focused? ph width cursor-style))
             (top-line (if (and top-b (> width 0)) (list (%make-h-border width)) '()))
             (bot-line (if (and bot-b (> width 0)) (list (%make-h-border width)) '()))
            ) ;
        (append top-line rendered bot-line)
      ) ;let*
    ) ;define

    (define (%editor-handle-input comp key)
      (let* ((data (component-data comp))
             (buf (%ed-buffer data))
             (hist (%ed-history data))
             (multi? (%ed-multiline? data))
             (on-ch (%ed-on-change data))
             (on-sub (%ed-on-submit data))
            ) ;
        (cond ((eq? key 'left) (buffer-move-left! buf))
              ((eq? key 'right) (buffer-move-right! buf))
              ((eq? key 'home) (buffer-move-home! buf))
              ((eq? key 'end) (buffer-move-end! buf))
              ((eq? key 'up)
               (if multi?
                 (buffer-move-up! buf)
                 (let ((prev-text (history-prev hist (buffer-text buf))))
                   (if prev-text
                     (begin
                       (buffer-set-text! buf prev-text)
                       (buffer-move-end! buf)
                       (when (procedure? on-ch)
                         (on-ch comp prev-text)
                       ) ;when
                       #t
                     ) ;begin
                     #f
                   ) ;if
                 ) ;let
               ) ;if
              ) ;
              ((eq? key 'down)
               (if multi?
                 (buffer-move-down! buf)
                 (let ((next-text (history-next hist (buffer-text buf))))
                   (if next-text
                     (begin
                       (buffer-set-text! buf next-text)
                       (buffer-move-end! buf)
                       (when (procedure? on-ch)
                         (on-ch comp next-text)
                       ) ;when
                       #t
                     ) ;begin
                     #f
                   ) ;if
                 ) ;let
               ) ;if
              ) ;
              ((eq? key 'backspace)
               (let ((res (buffer-backspace! buf)))
                 (when (and res (procedure? on-ch))
                   (on-ch comp (buffer-text buf))
                 ) ;when
                 res
               ) ;let
              ) ;
              ((eq? key 'delete)
               (let ((res (buffer-delete! buf)))
                 (when (and res (procedure? on-ch))
                   (on-ch comp (buffer-text buf))
                 ) ;when
                 res
               ) ;let
              ) ;
              ((eq? key 'enter)
               (if multi?
                 (let ((res (buffer-newline! buf)))
                   (when (procedure? on-ch)
                     (on-ch comp (buffer-text buf))
                   ) ;when
                   res
                 ) ;let
                 (let ((txt (buffer-text buf)))
                   (history-push! hist txt)
                   (when (procedure? on-sub)
                     (on-sub comp txt)
                   ) ;when
                   #t
                 ) ;let
               ) ;if
              ) ;
              ((string? key)
               (buffer-insert-string! buf key)
               (when (procedure? on-ch)
                 (on-ch comp (buffer-text buf))
               ) ;when
               #t
              ) ;
              ((char? key)
               (buffer-insert-char! buf key)
               (when (procedure? on-ch)
                 (on-ch comp (buffer-text buf))
               ) ;when
               #t
              ) ;
              (else #f)
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
