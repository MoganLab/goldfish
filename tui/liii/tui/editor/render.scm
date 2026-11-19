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

(define-library (liii tui editor render)
  (export editor-render)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii list)
    (liii unicode)
    (liii tui component)
    (liii tui text-width)
    (liii tui box border)
    (liii tui editor buffer)
    (liii tui editor state)
    (liii tui editor view)
    (liii tui editor viewport)
  ) ;import
  (begin

    ;; 无角纯水平线边框样式，复用 box border 渲染机制
    (define *editor-h-border-style* (make-border-style "" "" "" "" "─" ""))

    (define (%make-h-border width color)
      (let ((raw (border-render-top *editor-h-border-style* width)))
        (if (string? color) (string-append color raw "\x1b;[0m") raw)
      ) ;let
    ) ;define

    (define %editor-scroll-dim "\x1b;[2m")
    (define %editor-scroll-reset "\x1b;[0m")

    (define (%editor-scroll-line arrow n)
      (string-append %editor-scroll-dim
        arrow
        " "
        (number->string n)
        " more"
        %editor-scroll-reset
      ) ;string-append
    ) ;define

    (define (editor-render comp width)
      (let* ((data (component-data comp))
             (buf (%ed-buffer data))
             (raw-lines (buffer-lines buf))
             (row (buffer-row buf))
             (col (buffer-col buf))
             (focused? (%ed-focused? data))
             (ph (%ed-placeholder data))
             (cursor-style (%ed-cursor-style data))
             (top-b (%ed-top-border data))
             (bot-b (%ed-bottom-border data))
             (b-col (%ed-border-color data))
             ;; 内容折行预留最后一列给光标（对齐上游 layoutWidth = width - 1），
             ;; 避免写满最后一列触发真实终端 autowrap 待换行状态；
             ;; 视口同步传组件宽度，由 %ed-visual-lines 内部统一转换
             (layout-w (%editor-layout-width width))
             (vp (%ed-viewport data width))
             (top (car vp))
             (visible (cadr vp))
             (above (caddr vp))
             (below (cadddr vp))
             (placeholder? (and (not focused?) (= (length raw-lines) 1) (string=? (car raw-lines) ""))
             ) ;placeholder?
             (visual-lines (append-map (lambda (line) (wrap-text line layout-w)) raw-lines))
             (visible-lines (if placeholder? raw-lines (take (drop visual-lines top) visible))
             ) ;visible-lines
             (rendered
               (if placeholder?
                 (render-editor-lines visible-lines 0 0 #f ph layout-w cursor-style)
                 (if (and focused? (not (eq? cursor-style 'hardware)))
                   (let* ((pos (editor-visual-cursor raw-lines row col layout-w))
                          (cursor-row (- (car pos) top))
                          (cursor-col (cadr pos))
                         ) ;
                     (map (lambda (line r)
                            (if (= r cursor-row)
                              (render-line-with-cursor line cursor-col #t cursor-style)
                              line
                            ) ;if
                          ) ;lambda
                       visible-lines
                       (iota (length visible-lines))
                     ) ;map
                   ) ;let*
                   visible-lines
                 ) ;if
               ) ;if
             ) ;rendered
             (body
               (append (if (> above 0) (list (%editor-scroll-line "↑" above)) '())
                 rendered
                 (if (> below 0) (list (%editor-scroll-line "↓" below)) '())
               ) ;append
             ) ;body
            ) ;
        (if (and (> width 0) (or top-b bot-b))
          (append (if top-b (list (%make-h-border width b-col)) '())
            body
            (if bot-b (list (%make-h-border width b-col)) '())
          ) ;append
          body
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
