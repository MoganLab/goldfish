
(define-library (liii cmark stem blocks)
  (import (scheme base))
  (import (only (liii cmark parser) parse-fenced-code-block parse-indented-code-block)
  ) ;import
  (import (only (liii string) string-replace))
  (import (liii cmark inline math))
  (import (liii cmark block collector))
  (import (liii cmark block reference))
  (import (liii cmark stem util))
  (import (liii cmark stem inline))
  (import (liii cmark stem lists))
  (import (liii cmark stem blockquote))
  (import (liii cmark block line))
  (export parse-markdown-blocks)
  (begin
    ;; 块解析前展开每行前导 tab（CommonMark §2.1 Tabs，Example 1-11）；
    ;; 行内 tab 在后续 code/verbatim 上下文中保持字面
    (define (expand-line-tabs line)
      (expand-leading-tabs line)
    ) ;define

    (define (parse-markdown-blocks lines . maybe-refmap)
      (let ((expanded-lines (map expand-line-tabs lines)))
        (parse-markdown-blocks-with expanded-lines maybe-refmap)
      ) ;let
    ) ;define

    (define (parse-markdown-blocks-with lines maybe-refmap)
      (let ((refmap
              (if (null? maybe-refmap)
                (let ((new-refmap (make-reference-map)))
                  (collect-reference-definitions! lines new-refmap)
                  new-refmap
                ) ;let
                (car maybe-refmap)
              ) ;if
            ) ;refmap
           ) ;
        (let loop
          ((remaining lines) (blocks '()))
          (cond ((null? remaining) (reverse blocks))
                ((blank-or-whitespace-line? (car remaining)) (loop (cdr remaining) blocks))
                (else
                  (let ((definition (collect-link-reference-definition remaining)))
                    (if definition
                      (loop (cdr definition) blocks)
                      (let* ((parsed (collect-next-markdown-block remaining))
                             (parsed-ok? (car parsed))
                             (parsed-item (car parsed))
                             (stem (if parsed-ok? (markdown-block->stem parsed-item refmap) #f))
                            ) ;
                        (if (not stem) (reverse blocks) (loop (cdr parsed) (cons stem blocks)))
                      ) ;let*
                    ) ;if
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (collect-reference-definitions! lines refmap)
      (let loop
        ((remaining lines))
        (cond ((null? remaining) refmap)
              ((blank-or-whitespace-line? (car remaining)) (loop (cdr remaining)))
              (else
                (let ((definition (collect-link-reference-definition remaining)))
                  (if definition
                    (begin
                      (reference-map-add-definition! refmap
                        (car (car definition))
                        (car (cdr (car definition)))
                        (cdr (cdr (car definition)))
                      ) ;reference-map-add-definition!
                      (loop (cdr definition))
                    ) ;begin
                    (let ((parsed (collect-next-markdown-block remaining)))
                      (if (not (car parsed))
                        refmap
                        (begin
                          (collect-reference-definitions-in-block! (car parsed) refmap)
                          (loop (cdr parsed))
                        ) ;begin
                      ) ;if
                    ) ;let
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (collect-reference-definitions-in-block! block refmap)
      (case (markdown-block-type block)
            ((ordered-list bullet-list)
             (for-each (lambda (item-lines) (collect-reference-definitions! item-lines refmap))
               (markdown-block-lines block)
             ) ;for-each
            ) ;
            ((blockquote)
             (let ((lines (map unquote-blockquote-line (markdown-block-lines block))))
               (collect-reference-definitions! lines refmap)
             ) ;let
            ) ;
            (else refmap)
      ) ;case
    ) ;define

    (define (markdown-block->stem block refmap)
      (case (markdown-block-type block)
            ((thematic-break) (make-thematic-break-stem))
            ((ordered-list)
             (make-ordered-list-stem (markdown-block-data-ref block 'start 1)
               (markdown-block-data-ref block 'delimiter 'period)
               (markdown-block-data-ref block 'tightness 'tight)
               (markdown-block-lines block)
               (lambda (lines) (parse-markdown-blocks lines refmap))
             ) ;make-ordered-list-stem
            ) ;
            ((bullet-list)
             (make-bullet-list-stem (markdown-block-data-ref block 'tightness 'tight)
               (markdown-block-lines block)
               (lambda (lines) (parse-markdown-blocks lines refmap))
             ) ;make-bullet-list-stem
            ) ;
            ((blockquote)
             (make-blockquote-stem (markdown-block-lines block)
               (lambda (lines) (parse-markdown-blocks lines refmap))
             ) ;make-blockquote-stem
            ) ;
            ((alert)
             (make-alert-stem (markdown-block-data-ref block 'kind 'note)
               (markdown-block-lines block)
               refmap
             ) ;make-alert-stem
            ) ;
            ((table) (make-table-stem block refmap))
            ((html-block) (make-raw-block-stem (markdown-block-lines block)))
            ((math-block)
             (make-math-block-stem (parse-math-block (markdown-block-lines block)))
            ) ;
            ((fenced-code-block)
             (make-code-block-stem (parse-fenced-code-block (markdown-block-lines block)))
            ) ;
            ((indented-code-block)
             (make-code-block-stem (parse-indented-code-block (markdown-block-lines block)))
            ) ;
            ((setext-heading)
             (make-heading-stem (markdown-block-data-ref block 'level 1)
               (markdown-block-data-ref block 'content "")
               refmap
             ) ;make-heading-stem
            ) ;
            ((atx-heading)
             (make-heading-stem (markdown-block-data-ref block 'level 1)
               (markdown-block-data-ref block 'content "")
               refmap
             ) ;make-heading-stem
            ) ;
            (else (make-paragraph-stem (markdown-block-lines block) refmap))
      ) ;case
    ) ;define

    (define (make-heading-stem level line refmap)
      (cons (heading-symbol level) (inline->stem-list line refmap))
    ) ;define

    (define (make-paragraph-stem lines refmap)
      (cons 'p (inline->stem-list (string-join lines "\n") refmap))
    ) ;define

    ;; 代码块字面量中的 tab 展开为固定宽度空格（默认 4）。
    ;; 背景：Mogan 渲染 code 内容时无法解析字面 tab（stree->tree 后丢失），
    ;; 因此 md-stem 阶段就把 tab 展开，避免下游渲染问题。
    (define (expand-tabs-to-spaces str . maybe-width)
      (let ((width (if (null? maybe-width) 4 (car maybe-width))))
        (string-replace str "\t" (make-string width #\space))
      ) ;let
    ) ;define

    (define (make-code-block-stem node)
      (list 'code-block
        (node-data-ref node 'info "")
        (expand-tabs-to-spaces (node-data-ref node 'literal "") 4)
      ) ;list
    ) ;define

    (define (make-alert-stem kind lines refmap)
      (cons 'alert (cons kind (parse-markdown-blocks lines refmap)))
    ) ;define

    (define (make-raw-block-stem lines)
      (list 'raw-block "html" (string-join lines "\n"))
    ) ;define

    (define (make-math-block-stem node)
      (list 'math-block (node-data-ref node 'literal ""))
    ) ;define

    (define (make-table-stem block refmap)
      (let ((alignments (markdown-block-data-ref block 'alignments '()))
            (header (markdown-block-data-ref block 'header '()))
            (rows (markdown-block-data-ref block 'rows '()))
           ) ;
        (list 'table
          (cons 'align alignments)
          (list 'thead
            (cons 'tr (map (lambda (cell) (make-table-cell-stem 'th cell refmap)) header))
          ) ;list
          (cons 'tbody
            (map
              (lambda (row)
                (cons 'tr (map (lambda (cell) (make-table-cell-stem 'td cell refmap)) row))
              ) ;lambda
              rows
            ) ;map
          ) ;cons
        ) ;list
      ) ;let
    ) ;define

    (define (make-table-cell-stem tag cell refmap)
      (cons tag (inline->stem-list cell refmap))
    ) ;define

    (define (make-thematic-break-stem)
      '(thematic-break)
    ) ;define

    (define (heading-symbol level)
      (string->symbol (string-append "h" (number->string level)))
    ) ;define
  ) ;begin
) ;define-library
