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

(define-library (liii tui editor view)
  (export render-editor-lines render-line-with-cursor)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii list)
    (liii string)
    (liii unicode)
    (liii tui text-width)
  ) ;import
  (begin

    ;; 渲染带有光标指示的单行
    ;; focused?: 是否聚焦
    ;; cursor-style: 'inverse (反显), 'bar (竖线 |), 'block (方块或反显),
    ;; 'hardware (不画软件光标，由硬件光标指示)
    (define (render-line-with-cursor line col focused? cursor-style)
      (if (or (not focused?) (eq? cursor-style 'hardware))
        ;; 未聚焦或由硬件光标指示时原样返回，无需切分字符串
        line
        (let* ((len (utf8-string-length line)) (safe-col (max 0 (min col len))))
          (case cursor-style
           ((bar)
            (string-append (utf8-substring line 0 safe-col)
              "|"
              (if (>= safe-col len) "" (utf8-substring line safe-col len))
            ) ;string-append
           ) ;
           (else
             ;; 默认为反显样式 \x1b[7m ... \x1b[0m
             (let ((before (utf8-substring line 0 safe-col))
                   (after (if (>= (+ safe-col 1) len) "" (utf8-substring line (+ safe-col 1) len)))
                   (cursor-char (if (< safe-col len) (utf8-substring line safe-col (+ safe-col 1)) " ")
                   ) ;cursor-char
                  ) ;
               (string-append before "\x1b;[7m" cursor-char "\x1b;[0m" after)
             ) ;let
           ) ;else
          ) ;case
        ) ;let*
      ) ;if
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
          (map
            (lambda (line r)
              (if (and focused? (= r row))
                (render-line-with-cursor line col #t cursor-style)
                line
              ) ;if
            ) ;lambda
            lines
            (iota (length lines))
          ) ;map
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
