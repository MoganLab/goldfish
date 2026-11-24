;;
;; Copyright (C) 2026 Liii Network
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; md-stem 反向渲染的表格层：渲染表头、表体、对齐行和单元格，
;; 处理管道符转义与单元格内行内内容，并按终端显示宽度美化对齐。
;; 仅被 (liii cmark markdown blocks) 使用，不对最终用户开放。
(define-library (liii cmark markdown table)
  (import (scheme base))
  (import (liii cmark markdown util))
  (import (liii cmark markdown validate))
  (import (liii cmark markdown inline))
  (import (liii format unicode))
  (import (only (liii cmark text) string-join))
  (export render-table)
  (begin
    ;; 表格节点 → 行列表：所有行按各列最大终端显示宽度补齐空格。
    ;; 单元格内容和分隔线均包含在宽度计算中，每列至少保留三个连字符。
    (define (render-table node)
      (check-table-node node)
      (let ((alignments (cdr (list-ref node 1)))
            (header-row (cadr (list-ref node 2)))
            (body-rows (cdr (list-ref node 3)))
           ) ;
        (let ((rows
                (cons (render-row-cells (cdr header-row))
                  (map (lambda (row) (render-row-cells (cdr row))) body-rows)
                ) ;cons
              ) ;rows
             ) ;
          (let ((widths (table-column-widths rows)))
            (cons (render-row (car rows) widths)
              (cons (render-align-line alignments widths)
                (map (lambda (row) (render-row row widths)) (cdr rows))
              ) ;cons
            ) ;cons
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    (define (render-row-cells cells)
      (map render-cell cells)
    ) ;define

    (define (render-cell cell)
      (render-table-cell-inlines (cdr cell))
    ) ;define

    ;; 计算每列宽度。string-length 是字节长度，不能直接用于 UTF-8 中文；
    ;; string-terminal-width 按码点处理，宽字符计两列，组合字符计零列。
    (define (table-column-widths rows)
      (if (null? rows)
        '()
        (merge-column-widths (map rendered-cell-width (car rows))
          (table-column-widths (cdr rows))
        ) ;merge-column-widths
      ) ;if
    ) ;define

    (define (rendered-cell-width cell)
      (max 3 (string-terminal-width cell))
    ) ;define

    (define (merge-column-widths left right)
      (cond ((null? left) right)
            ((null? right) left)
            (else (cons (max (car left) (car right)) (merge-column-widths (cdr left) (cdr right)))
            ) ;else
      ) ;cond
    ) ;define

    ;; 对齐行：按列宽拉伸 default → ---，left → :--，center → :-:，right → --:
    (define (render-align-line alignments widths)
      (string-append "| "
        (string-join (render-align-cells alignments widths) " | ")
        " |"
      ) ;string-append
    ) ;define

    (define (render-align-cells alignments widths)
      (if (null? widths)
        '()
        (cons (render-align-cell (if (pair? alignments) (car alignments) 'default)
                (car widths)
              ) ;render-align-cell
          (render-align-cells (if (pair? alignments) (cdr alignments) '()) (cdr widths))
        ) ;cons
      ) ;if
    ) ;define

    (define (render-align-cell alignment width)
      (case alignment
       ((left) (string-append ":" (make-string (- width 1) #\-)))
       ((center) (string-append ":" (make-string (- width 2) #\-) ":"))
       ((right) (string-append (make-string (- width 1) #\-) ":"))
       (else (make-string width #\-))
      ) ;case
    ) ;define

    ;; 单行：| 单元格 | 单元格 |；右侧补空格使管道符纵向对齐。
    (define (render-row cells widths)
      (string-append "| " (string-join (render-padded-cells cells widths) " | ") " |")
    ) ;define

    (define (render-padded-cells cells widths)
      (if (null? widths)
        '()
        (let ((cell (if (pair? cells) (car cells) "")))
          (cons
            (string-append cell
              (make-string (- (car widths) (string-terminal-width cell)) #\space)
            ) ;string-append
            (render-padded-cells (if (pair? cells) (cdr cells) '()) (cdr widths))
          ) ;cons
        ) ;let
      ) ;if
    ) ;define
  ) ;begin
) ;define-library
