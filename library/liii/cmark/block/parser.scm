
(define-library (liii cmark block parser)
  (import (scheme base))
  (import (liii cmark block collector))
  (import (liii cmark block math))
  (import (liii cmark block list))
  (import (liii cmark node))
  (import (liii cmark inline math))
  (import (liii cmark text))
  (import (liii cmark inline entity))
  (import (liii cmark inline escape))
  (export blank-line?)
  (export blockquote-line?)
  (export unordered-list-item?)
  (export ordered-list-item?)
  (export ordered-list-delimiter)
  (export task-list-item-status)
  (export strip-task-list-item-marker)
  (export fenced-code-start?)
  (export thematic-break-line?)
  (export paragraph-continues?)
  (export block-continues?)
  (export parse-paragraph)
  (export parse-thematic-break)
  (export atx-heading-line?)
  (export parse-atx-heading)
  (export indented-code-line?)
  (export parse-indented-code-block)
  (export fenced-code-block-end?)
  (export parse-fenced-code-block)
  (export parse-list-item)
  (export parse-list)
  (export start-new-block)
  (export parse-document-lines)
  (export parse-blockquote)
  (export alert-block-start?)
  (export parse-alert-block)
  (export setext-underline?)
  (export setext-heading-level)
  (export parse-setext-heading)
  (export html-block-start?)
  (export parse-html-block)
  (begin
    (define (paragraph-continues? line)
      (and (not (blank-line? line))
        (not (atx-heading-line? line))
        (not (math-block-start-line? line))
        (not (html-block-start? line))
        (not (blockquote-line? line))
        (not (unordered-list-item? line))
        (not (ordered-list-item? line))
        (not (fenced-code-start? line))
        (not (thematic-break-line? line))
      ) ;and
    ) ;define

    (define (block-continues? block-type line)
      (case block-type
       ((paragraph) (paragraph-continues? line))
       (else #f)
      ) ;case
    ) ;define

    (define (parse-paragraph lines)
      (let ((para (make-cmark-paragraph)))
        (cmark-node-set-data! para `((content . ,(string-join lines "\n"))))
        para
      ) ;let
    ) ;define

    (define (parse-thematic-break)
      (make-cmark-node 'thematic_break '() '() #f)
    ) ;define

    (define (parse-atx-heading line)
      (let ((parsed (parse-atx-heading-line line)))
        (if parsed (make-cmark-heading (car parsed)) (make-cmark-heading 1))
      ) ;let
    ) ;define

    ;; 缩进已在 collect-indented-code-block-lines 剥除（Example 107-118）
    (define (parse-indented-code-block lines)
      (make-cmark-code-block "" (string-join lines "\n"))
    ) ;define

    ;; info 字符串解码实体与反斜杠转义（Example 24 的 \+ 、Example 34 的 &ouml;）；
    ;; 代码内容 verbatim，不解码
    (define (decode-fence-info raw-info)
      (decode-backslash-escapes-in-string (decode-entity-references-in-string raw-info)
      ) ;decode-backslash-escapes-in-string
    ) ;define

    (define (parse-fenced-code-block lines)
      (if (or (null? lines) (null? (cdr lines)))
        (make-cmark-code-block "" "")
        (let* ((fence (parse-fence-line (car lines)))
               (fence-char (car fence))
               (fence-len (cadr fence))
               (fence-indent (caddr fence))
               (info (decode-fence-info (cadddr fence)))
               (code-lines
                 (let loop
                   ((rest (cdr lines)))
                   (cond ((null? rest) '())
                         ((fenced-code-block-end? fence-char fence-len (car rest)) '())
                         (else
                           (let ((line (strip-code-indent fence-indent (car rest))))
                             (cons line (loop (cdr rest)))
                           ) ;let
                         ) ;else
                   ) ;cond
                 ) ;let
               ) ;code-lines
              ) ;
          (make-cmark-code-block info (string-join code-lines "\n"))
        ) ;let*
      ) ;if
    ) ;define

    (define (start-new-block line)
      (cond ((html-block-start? line) (make-cmark-html-block line))
            ((math-block-start-line? line) (parse-math-block (list line)))
            (else
              (markdown-block->cmark-node (car (collect-next-markdown-block (list line))))
            ) ;else
      ) ;cond
    ) ;define

    (define (parse-document-lines lines)
      (let ((doc (make-cmark-document)))
        (let loop
          ((remaining lines))
          (let ((next-lines (skip-leading-blank-lines remaining)))
            (cond ((null? next-lines) doc)
                  (else
                    (let ((parsed (collect-next-markdown-block next-lines)))
                      (cmark-node-append-child! doc (markdown-block->cmark-node (car parsed)))
                      (loop (cdr parsed))
                    ) ;let
                  ) ;else
            ) ;cond
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    (define (markdown-block->cmark-node block)
      (case (markdown-block-type block)
            ((thematic-break) (make-cmark-thematic-break))
            ((ordered-list)
             (make-cmark-list 'ordered
               (markdown-block-data-ref block 'start 1)
               (markdown-block-data-ref block 'delimiter 'period)
             ) ;make-cmark-list
            ) ;
            ((bullet-list) (make-cmark-list 'bullet 1 #f))
            ((blockquote) (parse-blockquote (markdown-block-lines block)))
            ((alert)
             (parse-alert-block (markdown-block-data-ref block 'kind 'note)
               (markdown-block-lines block)
             ) ;parse-alert-block
            ) ;
            ((html-block) (parse-html-block (markdown-block-lines block)))
            ((math-block) (parse-math-block (markdown-block-lines block)))
            ((table)
             (make-cmark-table (markdown-block-data-ref block 'alignments '())
               (markdown-block-data-ref block 'header '())
               (markdown-block-data-ref block 'rows '())
             ) ;make-cmark-table
            ) ;
            ((fenced-code-block) (parse-fenced-code-block (markdown-block-lines block)))
            ((indented-code-block) (parse-indented-code-block (markdown-block-lines block)))
            ((setext-heading) (make-cmark-heading (markdown-block-data-ref block 'level 1)))
            ((atx-heading) (make-cmark-heading (markdown-block-data-ref block 'level 1)))
            (else (parse-paragraph (markdown-block-lines block)))
      ) ;case
    ) ;define

    (define (parse-list lines)
      (if (null? lines)
        (make-cmark-list 'bullet 1 #f)
        (let* ((first-item (parse-list-item (car lines)))
               (first-kind (car first-item))
               (ordered-list? (and (pair? first-kind) (eq? (car first-kind) 'ordered)))
               (list-type (if ordered-list? 'ordered 'bullet))
               (start (if ordered-list? (cdr first-kind) 1))
               (delimiter (if ordered-list? (ordered-list-delimiter (car lines)) #f))
               (list-node (make-cmark-list list-type start delimiter))
              ) ;
          (let loop
            ((rest-lines lines) (items '()))
            (if (null? rest-lines)
              (begin
                (cmark-node-set-children! list-node (reverse items))
                list-node
              ) ;begin
              (let* ((parsed (parse-list-item (car rest-lines)))
                     (raw-content (cdr parsed))
                     (task-status (task-list-item-status raw-content))
                     (stripped (strip-task-list-item-marker raw-content))
                     (content (if task-status stripped raw-content))
                     (item (make-cmark-item))
                    ) ;
                (cmark-node-set-data! item
                  (if task-status
                    `((content . ,content) (task-status . ,task-status))
                    `((content . ,content))
                  ) ;if
                ) ;cmark-node-set-data!
                (loop (cdr rest-lines) (cons item items))
              ) ;let*
            ) ;if
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

    (define (parse-blockquote lines)
      (let* ((unquoted-lines (map unquote-blockquote-line lines))
             (content (string-join unquoted-lines "\n"))
             (bq (make-cmark-block-quote))
            ) ;
        (cmark-node-set-data! bq `((content . ,content)))
        bq
      ) ;let*
    ) ;define

    (define (parse-alert-block kind lines)
      (let ((alert (make-cmark-alert kind)) (content (string-join lines "\n")))
        (cmark-node-set-data! alert `((kind . ,kind) (content . ,content)))
        alert
      ) ;let
    ) ;define

    (define (parse-setext-heading lines)
      (if (or (null? lines) (null? (cdr lines)))
        (make-cmark-heading 1)
        (let* ((underline (cadr lines))
               (level (setext-heading-level underline))
               (heading (make-cmark-heading level))
              ) ;
          (cmark-node-set-data! heading `((level . ,level)
                                          (content . ,(car lines))))
          heading
        ) ;let*
      ) ;if
    ) ;define

    (define (parse-html-block lines)
      (let ((content (string-join lines "\n")))
        (make-cmark-html-block content)
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
