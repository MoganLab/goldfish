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

(define-library (liii tui editor view)
  (export render-editor-lines render-line-with-cursor)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii string)
    (liii tui text-width)
  ) ;import
  (begin

    ;; 渲染带有光标指示的单行
    ;; focused?: 是否聚焦
    ;; cursor-style: 'inverse (反显), 'bar (竖线 |), 'block (方块或反显), 'underline 等
    (define (render-line-with-cursor line col focused? cursor-style)
      (let* ((len (string-length line)) (safe-col (max 0 (min col len))))
        (if (not focused?)
          line
          (let ((before (substring line 0 safe-col))
                (after (if (< safe-col len) (substring line (+ safe-col 1) len) ""))
               ) ;
            (case cursor-style
             ((none hardware) line)
             ((bar)
              (string-append before
                "|"
                (if (< safe-col len) (substring line safe-col len) "")
              ) ;string-append
             ) ;
             (else
               ;; 默认为反显样式 \x1b[7m ... \x1b[0m
               (let ((cursor-char (if (< safe-col len) (string (string-ref line safe-col)) " ")))
                 (string-append before "\x1b;[7m" cursor-char "\x1b;[0m" after)
               ) ;let
             ) ;else
            ) ;case
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    ;; 渲染编辑器全部可见行
    ;; lines: 行列表
    ;; row, col: 光标位置
    ;; focused?: 聚焦状态
    ;; placeholder: 占位文本
    ;; width: 终端宽度限制
    ;; cursor-style: 光标风格
    (define (render-editor-lines lines row col focused? placeholder width cursor-style)
      (let* ((empty? (and (= (length lines) 1) (string=? (car lines) ""))))
        (if (and empty? (not (string=? placeholder "")) (not focused?))
          ;; 未聚焦且为空时显示占位文本
          (if (<= width 0) '("") (wrap-text placeholder width))
          (let loop
            ((rest lines) (r 0) (acc '()))
            (if (null? rest)
              (reverse acc)
              (let* ((line (car rest))
                     (line-rendered (if (and focused? (= r row))
                                      (render-line-with-cursor line col #t cursor-style)
                                      line
                                    ) ;if
                     ) ;line-rendered
                    ) ;
                (loop (cdr rest) (+ r 1) (cons line-rendered acc))
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
