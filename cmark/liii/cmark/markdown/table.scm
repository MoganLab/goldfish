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
;; 处理管道符转义与单元格内行内内容。行内代码中的管道符不需要
;; 转义（正向解析不会把代码内容按管道切分）。
;; 仅被 (liii cmark markdown blocks) 使用，不对最终用户开放。
(define-library (liii cmark markdown table)
  (import (scheme base))
  (import (liii cmark markdown util))
  (import (liii cmark markdown validate))
  (import (liii cmark markdown inline))
  (import (only (liii cmark text) string-join))
  (export render-table)
  (begin
    ;; 表格节点 → 行列表：表头行、对齐行、表体行
    (define (render-table node)
      (check-table-node node)
      (let ((alignments (cdr (list-ref node 1)))
            (header-row (cadr (list-ref node 2)))
            (body-rows (cdr (list-ref node 3)))
           ) ;
        (cons (render-row (car header-row) (cdr header-row))
          (cons (render-align-line alignments)
            (map (lambda (row) (render-row 'td (cdr row))) body-rows)
          ) ;cons
        ) ;cons
      ) ;let
    ) ;define

    ;; 对齐行：default → ---，left → :--，center → :-:，right → --:
    (define (render-align-line alignments)
      (string-append "|" (string-join (map render-align-cell alignments) "|") "|")
    ) ;define

    (define (render-align-cell alignment)
      (case alignment
       ((left) ":--")
       ((center) ":-:")
       ((right) "--:")
       (else "---")
      ) ;case
    ) ;define

    ;; 单行：| 单元格 | 单元格 |；单元格内管道符转义
    (define (render-row cell-tag cells)
      (string-append "|" (string-join (map render-cell cells) "|") "|")
    ) ;define

    (define (render-cell cell)
      (render-table-cell-inlines (cdr cell))
    ) ;define
  ) ;begin
) ;define-library
