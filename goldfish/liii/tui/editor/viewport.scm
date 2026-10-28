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

(define-library (liii tui editor viewport)
  (export %ed-sync-viewport! %ed-viewport)
  (import (scheme base)
    (liii list)
    (liii tui text-width)
    (liii tui editor buffer)
    (liii tui editor state)
    (liii tui editor view)
  ) ;import
  (begin

    (define (%ed-visual-lines data width)
      (append-map (lambda (line) (wrap-text line width))
        (buffer-lines (%ed-buffer data))
      ) ;append-map
    ) ;define

    (define (%ed-total-rows data width)
      (length (%ed-visual-lines data width))
    ) ;define

    ;; 可视内容行数：受 max-lines 限制，但不超过总行数
    (define (%ed-visible-count data width)
      (let ((total (%ed-total-rows data width)) (max-lines (%ed-max-lines data)))
        (cond ((not max-lines) total)
              ((< max-lines 1) 1)
              (else (min max-lines total))
        ) ;cond
      ) ;let
    ) ;define

    ;; 让光标保持在视口内：仅在越界时移动 top-row，窗口内移动不抖动
    (define (%ed-sync-viewport! data width)
      (let* ((total (%ed-total-rows data width))
             (visible (%ed-visible-count data width))
             (row
               (car (editor-visual-cursor (buffer-lines (%ed-buffer data))
                      (buffer-row (%ed-buffer data))
                      (buffer-col (%ed-buffer data))
                      width
                    ) ;editor-visual-cursor
               ) ;car
             ) ;row
             (max-top (max 0 (- total visible)))
             (top (max 0 (min (%ed-top-row data) max-top)))
             (new-top
               (cond ((< row top) row)
                     ((>= row (+ top visible)) (min max-top (+ 1 (- row visible))))
                     (else top)
               ) ;cond
             ) ;new-top
            ) ;
        (%ed-top-row-set! data (max 0 new-top))
        (%ed-top-row data)
      ) ;let*
    ) ;define

    ;; 返回 (top visible above below)，above/below 为隐藏行数（用于滚动提示）
    (define (%ed-viewport data width)
      (let* ((top (%ed-sync-viewport! data width))
             (total (%ed-total-rows data width))
             (visible (%ed-visible-count data width))
             (end (min total (+ top visible)))
            ) ;
        (list top visible top (- total end))
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
