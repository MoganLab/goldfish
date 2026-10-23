;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2026 The Goldfish Scheme Authors
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
    (liii tui component)
    (liii tui box border)
    (liii tui editor history)
    (liii tui editor buffer)
    (liii tui editor cursor)
    (liii tui editor kill)
    (liii tui editor view)
    (liii tui editor state)
  ) ;import
  (begin

    ;; 无角纯水平线边框样式，复用 box border 渲染机制
    (define *editor-h-border-style* (make-border-style "" "" "" "" "─" ""))

    (define (%make-h-border width color)
      (let ((raw (border-render-top *editor-h-border-style* width)))
        (if (string? color) (string-append color raw "\x1b;[0m") raw)
      ) ;let
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
             (b-col (%ed-border-color data))
             (rendered (render-editor-lines lines row col focused? ph width cursor-style))
            ) ;
        (if (and (> width 0) (or top-b bot-b))
          (append (if top-b (list (%make-h-border width b-col)) '())
            rendered
            (if bot-b (list (%make-h-border width b-col)) '())
          ) ;append
          rendered
        ) ;if
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
        (cond ((or (eq? key 'left) (eq? key 'ctrl-b)) (buffer-move-left! buf))
              ((or (eq? key 'right) (eq? key 'ctrl-f)) (buffer-move-right! buf))
              ((or (eq? key 'home) (eq? key 'ctrl-a) (eq? key 'ctrl-home))
               (buffer-move-home! buf)
              ) ;
              ((or (eq? key 'end) (eq? key 'ctrl-e) (eq? key 'ctrl-end))
               (buffer-move-end! buf)
              ) ;
              ((or (eq? key 'ctrl-left) (eq? key 'alt-b) (eq? key 'alt-left))
               (buffer-move-word-left! buf)
              ) ;
              ((or (eq? key 'ctrl-right) (eq? key 'alt-f) (eq? key 'alt-right))
               (buffer-move-word-right! buf)
              ) ;
              ((or (eq? key 'up) (eq? key 'ctrl-p))
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
              ((or (eq? key 'down) (eq? key 'ctrl-n))
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
              ((or (eq? key 'backspace) (eq? key 'ctrl-h))
               (let ((res (buffer-backspace! buf)))
                 (when (and res (procedure? on-ch))
                   (on-ch comp (buffer-text buf))
                 ) ;when
                 res
               ) ;let
              ) ;
              ((or (eq? key 'delete) (eq? key 'ctrl-d))
               (let ((res (buffer-delete! buf)))
                 (when (and res (procedure? on-ch))
                   (on-ch comp (buffer-text buf))
                 ) ;when
                 res
               ) ;let
              ) ;
              ((eq? key 'ctrl-k)
               (let ((res (buffer-kill-line-right! buf)))
                 (when (and res (procedure? on-ch))
                   (on-ch comp (buffer-text buf))
                 ) ;when
                 res
               ) ;let
              ) ;
              ((eq? key 'ctrl-u)
               (let ((res (buffer-kill-line-left! buf)))
                 (when (and res (procedure? on-ch))
                   (on-ch comp (buffer-text buf))
                 ) ;when
                 res
               ) ;let
              ) ;
              ((or (eq? key 'ctrl-w) (eq? key 'alt-backspace))
               (let ((res (buffer-kill-word-left! buf)))
                 (when (and res (procedure? on-ch))
                   (on-ch comp (buffer-text buf))
                 ) ;when
                 res
               ) ;let
              ) ;
              ((eq? key 'ctrl-t)
               (let ((res (buffer-transpose-chars! buf)))
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
