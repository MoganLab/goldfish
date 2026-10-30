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
  (export render-editor-lines render-line-with-cursor editor-visual-cursor
    %editor-layout-width
  ) ;export
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii list)
    (liii string)
    (liii unicode)
    (liii tui text-width)
  ) ;import
  (begin

    ;; 编辑器内容布局宽度：无 padding 时预留最后一列给光标，
    ;; 对齐上游 pi editor.ts 的 layoutWidth = contentWidth - 1 设计。
    ;; 内容永不写满整行，避免真实终端写满最后一列后进入 autowrap
    ;; 待换行状态、随后的清行序列擦掉最后一列字符（Windows 上吞字）。
    ;; width <= 1 时原样返回：0 是测试与初始化的哨兵宽度。
    (define (%editor-layout-width width)
      (if (<= width 1) width (- width 1))
    ) ;define

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

    ;; 统计 str 前 end 个字符中末尾连续 ASCII 空格的个数
    (define (%count-tail-spaces str end)
      (let loop
        ((i end) (n 0))
        (if (<= i 0)
          n
          (if (string=? (utf8-substring str (- i 1) i) " ") (loop (- i 1) (+ n 1)) n)
        ) ;if
      ) ;let
    ) ;define

    ;; 将逻辑光标转换为折行后的视觉行和显示列。
    (define (editor-visual-cursor lines row col width)
      (let loop
        ((rest lines) (r 0) (visual-row 0))
        (if (null? rest)
          (list visual-row 0)
          (let ((parts (wrap-text (car rest) width)))
            (if (= r row)
              (let* ((line (car rest))
                     (safe-col (max 0 (min col (utf8-string-length line))))
                     (prefix-parts (wrap-text (utf8-substring line 0 safe-col) width))
                     (last-part (car (reverse prefix-parts)))
                     ;; wrap-text 会去掉段尾空格：未折行时直接计算原始前缀宽度，
                     ;; 已折行时把最后一段之后的行尾空格补回，避免光标位于
                     ;; 空格之后时显示列少算。
                     (raw-col (if (= (length prefix-parts) 1)
                                (display-width-prefix line safe-col)
                                (+ (display-width last-part) (%count-tail-spaces line safe-col))
                              ) ;if
                     ) ;raw-col
                     ;; 宽度已是布局宽度（内容最多占 0..width-1）：
                     ;; 行尾光标 raw-col = width 恰好落在预留列（组件最后一列），
                     ;; 合法保留；仅 raw-col 超过预留列（尾部空格补回/宽字符越界）
                     ;; 时夹到预留列。光标后有内容时仍由下方换行惩罚分支
                     ;; 移至下一行行首。
                     (display-col (if (and (> width 0) (> raw-col width)) width raw-col))
                     (prefix-row (+ visual-row (- (length prefix-parts) 1)))
                    ) ;
                (if (and (> width 0) (< safe-col (utf8-string-length line)) (>= raw-col width))
                  (list (+ prefix-row 1) 0)
                  (list prefix-row display-col)
                ) ;if
              ) ;let*
              (loop (cdr rest) (+ r 1) (+ visual-row (length parts)))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
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
          (wrap-text placeholder width)
          (append-map
            (lambda (line r)
              (let ((parts (wrap-text line width)))
                (if (and focused? (= r row))
                  (let* ((pos (editor-visual-cursor (list line) 0 col width))
                         (cursor-row (car pos))
                         (cursor-col (cadr pos))
                        ) ;
                    (map (lambda (part part-row)
                           (if (= part-row cursor-row)
                             (render-line-with-cursor part cursor-col #t cursor-style)
                             part
                           ) ;if
                         ) ;lambda
                      parts
                      (iota (length parts))
                    ) ;map
                  ) ;let*
                  parts
                ) ;if
              ) ;let
            ) ;lambda
            lines
            (iota (length lines))
          ) ;append-map
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
