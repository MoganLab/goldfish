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

(define-library (liii tui editor state)
  (export <editor-data> %make-editor-data %editor-data? %ed-buffer
    %ed-buffer-set! %ed-history %ed-history-set! %ed-multiline?
    %ed-multiline?-set! %ed-focused? %ed-focused?-set! %ed-placeholder
    %ed-placeholder-set! %ed-cursor-style %ed-cursor-style-set! %ed-on-submit
    %ed-on-submit-set! %ed-on-change %ed-on-change-set! %ed-top-border
    %ed-top-border-set! %ed-bottom-border %ed-bottom-border-set!
    %ed-border-color %ed-border-color-set!
  ) ;export
  (import (scheme base))
  (begin

    (define-record-type <editor-data>
      (%make-editor-data buffer history multiline? focused? placeholder
        cursor-style on-submit on-change top-border bottom-border border-color
      ) ;%make-editor-data
      %editor-data?
      (buffer %ed-buffer %ed-buffer-set!)
      (history %ed-history %ed-history-set!)
      (multiline? %ed-multiline? %ed-multiline?-set!)
      (focused? %ed-focused? %ed-focused?-set!)
      (placeholder %ed-placeholder %ed-placeholder-set!)
      (cursor-style %ed-cursor-style %ed-cursor-style-set!)
      (on-submit %ed-on-submit %ed-on-submit-set!)
      (on-change %ed-on-change %ed-on-change-set!)
      (top-border %ed-top-border %ed-top-border-set!)
      (bottom-border %ed-bottom-border %ed-bottom-border-set!)
      (border-color %ed-border-color %ed-border-color-set!)
    ) ;define-record-type

    (define <editor-data> '<editor-data>)

  ) ;begin
) ;define-library
