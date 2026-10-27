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
    (liii unicode)
    (liii tui component)
    (liii tui box border)
    (liii tui editor history)
    (liii tui editor buffer)
    (liii tui editor cursor)
    (liii tui editor kill)
    (liii tui editor view)
    (liii tui editor state)
    (liii tui editor paste)
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

    (define (%apply-edit! buf comp on-ch thunk)
      (let ((res (thunk)))
        (when (and res (procedure? on-ch))
          (on-ch comp (buffer-text buf))
        ) ;when
        res
      ) ;let
    ) ;define

    (define (%load-history! buf comp on-ch text)
      (when text
        (buffer-set-text! buf text)
        (buffer-move-end! buf)
        (when (procedure? on-ch)
          (on-ch comp text)
        ) ;when
      ) ;when
      (if text #t #f)
    ) ;define

    ;; -----------------------------------------------------------------
    ;; 粘贴：大段内容折叠为 [paste #N ...]，提交时由 paste-expand 展开
    ;; -----------------------------------------------------------------

    (define (%handle-paste! comp data buf on-ch text)
      (let ((clean (%paste-normalize text)))
        (if (string=? clean "")
          #t
          (begin
            (if (%paste-large? clean)
              (buffer-insert-text! buf (paste-registry-add! (%ed-pastes data) clean))
              (buffer-insert-text! buf clean)
            ) ;if
            (when (procedure? on-ch)
              (on-ch comp (buffer-text buf))
            ) ;when
            #t
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    (define (%current-line buf)
      (list-ref (buffer-lines buf) (buffer-row buf))
    ) ;define

    (define (%remove-span! buf start end)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (line (list-ref lines r))
             (len (utf8-string-length line))
             (before (if (<= start 0) "" (utf8-substring line 0 start)))
             (after (if (>= end len) "" (utf8-substring line end len)))
             (new-line (string-append before after))
            ) ;
        (buffer-lines-set! buf (%replace-line lines r new-line))
        #t
      ) ;let*
    ) ;define

    ;; 光标前是有效标记时整体退格删除，并重排后续编号
    (define (%try-backspace-marker! comp data buf on-ch)
      (let* ((reg (%ed-pastes data))
             (line (%current-line buf))
             (col (buffer-col buf))
             (span (paste-marker-at-end reg line col))
            ) ;
        (when span
          (let ((id (car span)) (start (cdr span)))
            (%remove-span! buf start col)
            (paste-registry-remove! reg id)
            (buffer-lines-set! buf (paste-renumber-lines (buffer-lines buf) id))
            (buffer-col-set! buf (min start (utf8-string-length (%current-line buf))))
            (when (procedure? on-ch)
              (on-ch comp (buffer-text buf))
            ) ;when
          ) ;let
        ) ;when
        (if span #t #f)
      ) ;let*
    ) ;define

    ;; Delete 键删除光标处的有效标记（编号不重排，与上游一致）
    (define (%try-delete-marker! comp data buf on-ch)
      (let* ((reg (%ed-pastes data))
             (line (%current-line buf))
             (col (buffer-col buf))
             (span (paste-marker-at-start reg line col))
            ) ;
        (when span
          (%remove-span! buf col (cdr span))
          (when (procedure? on-ch)
            (on-ch comp (buffer-text buf))
          ) ;when
        ) ;when
        (if span #t #f)
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
               ;; 粘贴后即使编辑器是单行模式，只要缓冲区有多行也应能上下移动
               (if
                 (or multi? (> (length (buffer-lines buf)) 1))
                 (buffer-move-up! buf)
                 (%load-history! buf comp on-ch (history-prev hist (buffer-text buf)))
               ) ;if
              ) ;
              ((or (eq? key 'down) (eq? key 'ctrl-n))
               (if
                 (or multi? (> (length (buffer-lines buf)) 1))
                 (buffer-move-down! buf)
                 (%load-history! buf comp on-ch (history-next hist (buffer-text buf)))
               ) ;if
              ) ;
              ((or (eq? key 'backspace) (eq? key 'ctrl-h))
               (or (%try-backspace-marker! comp data buf on-ch)
                 (%apply-edit! buf comp on-ch (lambda () (buffer-backspace! buf)))
               ) ;or
              ) ;
              ((or (eq? key 'delete) (eq? key 'ctrl-d))
               (or (%try-delete-marker! comp data buf on-ch)
                 (%apply-edit! buf comp on-ch (lambda () (buffer-delete! buf)))
               ) ;or
              ) ;
              ((eq? key 'ctrl-k)
               (%apply-edit! buf comp on-ch (lambda () (buffer-kill-line-right! buf)))
              ) ;
              ((eq? key 'ctrl-u)
               (%apply-edit! buf comp on-ch (lambda () (buffer-kill-line-left! buf)))
              ) ;
              ((or (eq? key 'ctrl-w) (eq? key 'alt-backspace))
               (%apply-edit! buf comp on-ch (lambda () (buffer-kill-word-left! buf)))
              ) ;
              ((eq? key 'ctrl-t)
               (%apply-edit! buf comp on-ch (lambda () (buffer-transpose-chars! buf)))
              ) ;
              ((eq? key 'enter)
               (if multi?
                 (let ((res (buffer-newline! buf)))
                   (when (procedure? on-ch)
                     (on-ch comp (buffer-text buf))
                   ) ;when
                   res
                 ) ;let
                 ;; 提交前展开隐藏的粘贴内容
                 (let ((txt (paste-expand (%ed-pastes data) (buffer-text buf))))
                   (history-push! hist txt)
                   (when (procedure? on-sub)
                     (on-sub comp txt)
                   ) ;when
                   #t
                 ) ;let
               ) ;if
              ) ;
              ((and (string? key) (%bracketed-paste? key))
               (%handle-paste! comp data buf on-ch (%bracketed-paste-content key))
              ) ;
              ((and (string? key)
                 (or (> (utf8-string-length key) 1)
                   (string=? key "\n")
                   (string=? key "\r")
                   (string=? key "\t")
                 ) ;or
               ) ;and
               ;; 多字符/带换行的字符串按粘贴处理：统一归一化，超过阈值才折叠
               (%handle-paste! comp data buf on-ch key)
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
