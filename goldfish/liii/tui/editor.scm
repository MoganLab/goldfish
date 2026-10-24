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

(define-library (liii tui editor)
  (export make-editor
    editor?
    make-text-input
    text-input?
    make-text-area
    text-area?
    editor-text
    editor-set-text!
    editor-cursor
    editor-set-cursor!
    editor-display-col
    editor-lines
    editor-focused?
    editor-set-focused!
    editor-placeholder
    editor-set-placeholder!
    editor-history-list
    editor-history
    editor-set-on-submit!
    editor-border-color
    editor-set-border-color!
    ;; buffer 导出
    make-buffer
    buffer?
    buffer-lines
    buffer-row
    buffer-col
    buffer-multiline?
    buffer-text
    buffer-set-text!
    buffer-insert-char!
    buffer-insert-string!
    buffer-backspace!
    buffer-delete!
    buffer-newline!
    ;; cursor 导出
    buffer-move-left!
    buffer-move-right!
    buffer-move-up!
    buffer-move-down!
    buffer-move-home!
    buffer-move-end!
    buffer-move-word-left!
    buffer-move-word-right!
    ;; kill / edit 导出
    buffer-kill-line-right!
    buffer-kill-line-left!
    buffer-kill-word-left!
    buffer-transpose-chars!
    ;; history 导出
    make-history
    history?
    history-entries
    history-max-size
    history-index
    history-draft
    history-push!
    history-prev
    history-next
    history-reset!
    ;; view 导出
    render-editor-lines
    render-line-with-cursor
  ) ;export
  (import (liii tui editor history)
    (liii tui editor buffer)
    (liii tui editor cursor)
    (liii tui editor kill)
    (liii tui editor view)
    (liii tui editor component)
  ) ;import
) ;define-library
