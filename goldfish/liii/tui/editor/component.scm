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

(define-library (liii tui editor component)
  (export make-editor editor? make-text-input text-input? make-text-area
    text-area? editor-text editor-set-text! editor-cursor editor-set-cursor!
    editor-display-col editor-display-position editor-lines editor-focused?
    editor-set-focused! editor-placeholder editor-set-placeholder!
    editor-history-list editor-history editor-set-on-submit! editor-border-color
    editor-set-border-color! editor-max-lines editor-set-max-lines!
    editor-top-row editor-viewport editor-expanded-text
  ) ;export
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii error)
    (liii tui text-width)
    (liii tui component)
    (liii tui editor history)
    (liii tui editor buffer)
    (liii tui editor cursor)
    (liii tui editor state)
    (liii tui editor view)
    (liii tui editor viewport)
    (liii tui editor paste)
    (liii tui editor handle)
    (liii tui editor render)
    (liii tui editor undo)
  ) ;import
  (begin

    (define (editor? obj)
      (and (component? obj) (%editor-data? (component-data obj)))
    ) ;define

    (define text-input? editor?)
    (define text-area? editor?)

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

    (define (editor-text ed)
      (unless (editor? ed)
        (type-error "editor-text: ed must be an editor")
      ) ;unless
      (buffer-text (%ed-buffer (component-data ed)))
    ) ;define

    (define (editor-set-text! ed text)
      (unless (editor? ed)
        (type-error "editor-set-text!: ed must be an editor")
      ) ;unless
      (let* ((data (component-data ed)) (buf (%ed-buffer data)))
        (unless (string=? (buffer-text buf) text)
          ;; 外部整体替换（含清空）可撤销：先记录替换前快照
          (%ed-undo-stack-set! data (cons (%editor-snapshot data) (%ed-undo-stack data)))
          (%ed-last-action-set! data #f)
        ) ;unless
        (buffer-set-text! buf text)
        ;; 外部整体替换文本时，旧的粘贴隐藏内容与编号不再适用
        (paste-registry-clear! (%ed-pastes data))
        (when (procedure? (%ed-on-change data))
          ((%ed-on-change data) ed text)
        ) ;when
      ) ;let*
    ) ;define

    ;; 提交/外部读取用：把 [paste #N ...] 标记展开成真实内容
    (define (editor-expanded-text ed)
      (unless (editor? ed)
        (type-error "editor-expanded-text: ed must be an editor")
      ) ;unless
      (let ((data (component-data ed)))
        (paste-expand (%ed-pastes data) (buffer-text (%ed-buffer data)))
      ) ;let
    ) ;define

    (define (editor-lines ed)
      (unless (editor? ed)
        (type-error "editor-lines: ed must be an editor")
      ) ;unless
      (buffer-lines (%ed-buffer (component-data ed)))
    ) ;define

    (define (editor-cursor ed)
      (unless (editor? ed)
        (type-error "editor-cursor: ed must be an editor")
      ) ;unless
      (let ((buf (%ed-buffer (component-data ed))))
        (list (buffer-row buf) (buffer-col buf))
      ) ;let
    ) ;define

    (define (editor-set-cursor! ed row col)
      (unless (editor? ed)
        (type-error "editor-set-cursor!: ed must be an editor")
      ) ;unless
      (let* ((buf (%ed-buffer (component-data ed)))
             (lines (buffer-lines buf))
             (total-rows (length lines))
             (safe-row (max 0 (min row (- total-rows 1))))
             (line (list-ref lines safe-row))
             (safe-col (max 0 (min col (utf8-string-length line))))
            ) ;
        (buffer-row-set! buf safe-row)
        (buffer-col-set! buf safe-col)
      ) ;let*
    ) ;define

    (define (editor-display-col ed)
      (unless (editor? ed)
        (type-error "editor-display-col: ed must be an editor")
      ) ;unless
      (let* ((buf (%ed-buffer (component-data ed)))
             (lines (buffer-lines buf))
             (r (buffer-row buf))
             (c (buffer-col buf))
             (line
               (if (and (>= r 0) (< r (length lines))) (list-ref lines r) "")
             ) ;line
             (safe-c (max 0 (min c (utf8-string-length line))))
            ) ;
        (display-width-prefix line safe-c)
      ) ;let*
    ) ;define

    (define (editor-focused? ed)
      (unless (editor? ed)
        (type-error "editor-focused?: ed must be an editor")
      ) ;unless
      (%ed-focused? (component-data ed))
    ) ;define

    (define (editor-set-focused! ed focused?)
      (unless (editor? ed)
        (type-error "editor-set-focused!: ed must be an editor")
      ) ;unless
      (%ed-focused?-set! (component-data ed) (and focused? #t))
    ) ;define

    (define (editor-placeholder ed)
      (unless (editor? ed)
        (type-error "editor-placeholder: ed must be an editor")
      ) ;unless
      (%ed-placeholder (component-data ed))
    ) ;define

    (define (editor-set-placeholder! ed ph)
      (unless (editor? ed)
        (type-error "editor-set-placeholder!: ed must be an editor")
      ) ;unless
      (%ed-placeholder-set! (component-data ed) ph)
    ) ;define

    (define (editor-max-lines ed)
      (unless (editor? ed)
        (type-error "editor-max-lines: ed must be an editor")
      ) ;unless
      (%ed-max-lines (component-data ed))
    ) ;define

    (define (editor-set-max-lines! ed max-lines)
      (unless (editor? ed)
        (type-error "editor-set-max-lines!: ed must be an editor")
      ) ;unless
      (unless (or (not max-lines) (and (integer? max-lines) (> max-lines 0)))
        (type-error "editor-set-max-lines!: max-lines must be #f or a positive integer")
      ) ;unless
      (%ed-max-lines-set! (component-data ed) max-lines)
      (%ed-sync-viewport! (component-data ed) 0)
      #t
    ) ;define

    ;; 返回 (top visible above below)：视口首行、可视行数、上方/下方隐藏行数
    (define (editor-viewport ed . width-arg)
      (unless (editor? ed)
        (type-error "editor-viewport: ed must be an editor")
      ) ;unless
      (%ed-viewport (component-data ed) (if (pair? width-arg) (car width-arg) 0))
    ) ;define

    ;; 返回相对当前视口的视觉光标位置 (row col)
    (define (editor-display-position ed width)
      (unless (editor? ed)
        (type-error "editor-display-position: ed must be an editor")
      ) ;unless
      (let* ((data (component-data ed))
             (buf (%ed-buffer data))
             (vp (%ed-viewport data width))
             (pos (editor-visual-cursor (buffer-lines buf)
                    (buffer-row buf)
                    (buffer-col buf)
                    width
                  ) ;editor-visual-cursor
             ) ;pos
            ) ;
        (list (- (car pos) (car vp)) (cadr pos))
      ) ;let*
    ) ;define

    (define (editor-top-row ed)
      (unless (editor? ed)
        (type-error "editor-top-row: ed must be an editor")
      ) ;unless
      (%ed-top-row (component-data ed))
    ) ;define

    (define (editor-border-color ed)
      (unless (editor? ed)
        (type-error "editor-border-color: ed must be an editor")
      ) ;unless
      (%ed-border-color (component-data ed))
    ) ;define

    (define (editor-set-border-color! ed color)
      (unless (editor? ed)
        (type-error "editor-set-border-color!: ed must be an editor")
      ) ;unless
      (%ed-border-color-set! (component-data ed) color)
    ) ;define

    (define (editor-history-list ed)
      (unless (editor? ed)
        (type-error "editor-history-list: ed must be an editor")
      ) ;unless
      (history-entries (%ed-history (component-data ed)))
    ) ;define

    (define (editor-history ed)
      (unless (editor? ed)
        (type-error "editor-history: ed must be an editor")
      ) ;unless
      (%ed-history (component-data ed))
    ) ;define

    (define (editor-set-on-submit! ed proc)
      (unless (editor? ed)
        (type-error "editor-set-on-submit!: ed must be an editor")
      ) ;unless
      (unless (or (not proc) (procedure? proc))
        (type-error "editor-set-on-submit!: proc must be a procedure or #f")
      ) ;unless
      (%ed-on-submit-set! (component-data ed) proc)
    ) ;define

    (define (make-editor . args)
      (let* ((initial-text
               (if (and (pair? args) (string? (car args))) (car args) "")
             ) ;initial-text
             (rest-opts
               (if (and (pair? args) (string? (car args))) (cdr args) args)
             ) ;rest-opts
             (multiline? (%keyword-value :multiline rest-opts #f))
             ;; Enter 提交与多行换行通过该信号解耦
             (submit-on-enter? (%keyword-value :submit-on-enter? rest-opts #t))
             (max-lines (%keyword-value :max-lines rest-opts #f))
             (focused? (%keyword-value :focused rest-opts #t))
             (placeholder (%keyword-value :placeholder rest-opts ""))
             (cursor-style (%keyword-value :cursor-style rest-opts 'inverse))
             (on-submit (%keyword-value :on-submit rest-opts #f))
             (on-change (%keyword-value :on-change rest-opts #f))
             (top-border (%keyword-value :top-border rest-opts #f))
             (bottom-border (%keyword-value :bottom-border rest-opts #f))
             (border-color (%keyword-value :border-color rest-opts #f))
             (history-size (%keyword-value :history-size rest-opts 100))
             (buf (make-buffer initial-text multiline?))
             (hist (make-history history-size))
             (data (%make-editor-data buf
                     hist
                     multiline?
                     focused?
                     placeholder
                     cursor-style
                     on-submit
                     on-change
                     top-border
                     bottom-border
                     border-color
                     submit-on-enter?
                     max-lines
                     0
                     (make-paste-registry)
                     '()
                     #f
                   ) ;%make-editor-data
             ) ;data
             (comp (make-component editor-render editor-handle-input))
            ) ;
        (unless (or (not max-lines) (and (integer? max-lines) (> max-lines 0)))
          (type-error "make-editor: :max-lines must be #f or a positive integer")
        ) ;unless
        (component-data-set! comp data)
        (buffer-move-end! buf)
        comp
      ) ;let*
    ) ;define

    (define (make-text-input . args)
      (let* ((initial-text
               (if (and (pair? args) (string? (car args))) (car args) "")
             ) ;initial-text
             (rest-opts
               (if (and (pair? args) (string? (car args))) (cdr args) args)
             ) ;rest-opts
            ) ;
        (apply make-editor (append (list initial-text :multiline #f) rest-opts))
      ) ;let*
    ) ;define

    (define (make-text-area . args)
      (let* ((initial-text
               (if (and (pair? args) (string? (car args))) (car args) "")
             ) ;initial-text
             (rest-opts
               (if (and (pair? args) (string? (car args))) (cdr args) args)
             ) ;rest-opts
            ) ;
        (apply make-editor (append (list initial-text :multiline #t) rest-opts))
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
