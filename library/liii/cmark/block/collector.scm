
(define-library (liii cmark block collector)
  (import (scheme base) (liii base))
  (import (scheme char))
  (import (liii cmark block blockquote))
  (import (liii cmark block alert))
  (import (liii cmark block list))
  (import (liii cmark block math))
  (import (liii cmark block html))
  (import (liii cmark block table))
  (import (liii cmark text))
  (import (liii cmark block line))
  (import (liii cmark block heading-code))
  (import (liii cmark block list-collect))
  (export make-markdown-block markdown-block? markdown-block-type
    markdown-block-lines markdown-block-data markdown-block-data-ref blank-line?
    skip-leading-blank-lines blockquote-line? collect-blockquote-lines
    collect-blockquote-lines-with-lazy unquote-blockquote-line
    alert-block-start? collect-alert-block-lines math-block-start-line?
    collect-math-block-lines html-block-start? html-block-interrupts-paragraph?
    collect-html-block-lines fenced-code-start? fenced-code-block-end?
    parse-fence-line collect-fenced-code-block-lines strip-code-indent
    indented-code-line? collect-indented-code-block-lines thematic-break-line?
    setext-underline? setext-heading-level setext-heading-start?
    parse-atx-heading-line atx-heading-line? table-start?
    collect-paragraph-lines block-start? collect-next-markdown-block
  ) ;export
  (begin
    (define*-record-type <markdown-block>
      (make-markdown-block type (lines '()) (data '()))
      markdown-block?
      (type markdown-block-type)
      (lines markdown-block-lines)
      (data markdown-block-data)
    ) ;define*-record-type

    (define (markdown-block-data-ref block key default)
      (assoc-ref-default (markdown-block-data block) key default)
    ) ;define

    (define (skip-leading-blank-lines lines)
      (cond ((null? lines) '())
            ((blank-line? (car lines)) (skip-leading-blank-lines (cdr lines)))
            (else lines)
      ) ;cond
    ) ;define

    (define (list-item-interrupts-paragraph? line)
      (let ((marker (parse-markdown-list-marker line)))
        (and marker
          (not (string=? (markdown-list-marker-body marker) ""))
          (or (eq? (markdown-list-marker-type marker) 'bullet)
            (= (markdown-list-marker-start marker) 1)
          ) ;or
        ) ;and
      ) ;let
    ) ;define

    ;; 段落中断判定（不含 setext，由 collect-paragraph-lines 处理）
    (define (block-start? lines)
      (or (math-block-start-line? (car lines))
        (html-block-interrupts-paragraph? (car lines))
        (list-item-interrupts-paragraph? (car lines))
        (blockquote-line? (car lines))
        (atx-heading-line? (car lines))
        (fenced-code-start? (car lines))
        (thematic-break-line? (car lines))
        (table-start? lines)
      ) ;or
    ) ;define

    ;; 单行块起始判定（惰性续行用，不含 table 的多行检查）
    (define (line-starts-block? line)
      (or (math-block-start-line? line)
        (html-block-interrupts-paragraph? line)
        (list-item-interrupts-paragraph? line)
        (blockquote-line? line)
        (atx-heading-line? line)
        (fenced-code-start? line)
        (thematic-break-line? line)
      ) ;or
    ) ;define

    ;; 引用内容行的递归判定：剥掉 > / 列表标记后是否为开放的段落行
    ;; （> > foo 的最内层段落仍开放 → 允许惰性续行，Example 250/292）
    (define (line-ends-open-paragraph? line)
      (cond ((blank-or-whitespace-line? line) #f)
            ;; 4 空格及以上 → 缩进代码块，段落不开放（Example 236）
            ((indented-code-line? line) #f)
            ((blockquote-line? line)
             (line-ends-open-paragraph? (unquote-blockquote-line line))
            ) ;
            (else
              (let ((item (parse-list-item line)))
                (if
                  (and item (not (string=? (cdr item) line)))
                  (line-ends-open-paragraph? (cdr item))
                  (not (line-starts-block? line))
                ) ;if
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; 惰性续行不能成为 setext 下划线（Example 93）：转义为段落文本
    (define (escape-lazy-setext-line line)
      (if (setext-underline? line) (string-append "\\" line) line)
    ) ;define

    ;; 引用块收集（含惰性续行，Example 232/233/238/247/250/251）：
    ;; 最后一个 > 行的最内层段落仍开放、且候选行不开启新块时，无 > 行并入引用
    (define (collect-blockquote-lines-with-lazy lines)
      (let loop
        ((remaining lines) (collected '()))
        (cond ((null? remaining) (cons (reverse collected) '()))
              ((blockquote-line? (car remaining))
               (loop (cdr remaining) (cons (car remaining) collected))
              ) ;
              ((blank-or-whitespace-line? (car remaining))
               (cons (reverse collected) remaining)
              ) ;
              ((and (line-ends-open-paragraph? (car collected)) (not (block-start? remaining)))
               (loop (cdr remaining)
                 (cons (escape-lazy-setext-line (car remaining)) collected)
               ) ;loop
              ) ;
              (else (cons (reverse collected) remaining))
        ) ;cond
      ) ;let
    ) ;define

    ;; 段落续行的前导空白全部去掉（CommonMark Example 49）
    (define (strip-line-leading-whitespace line)
      (let ((len (string-length line)))
        (let loop
          ((i 0))
          (if
            (and (< i len)
              (or (char=? (string-ref line i) #\space) (char=? (string-ref line i) #\tab))
            ) ;and
            (loop (+ i 1))
            (substring line i len)
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 段落末行行尾空白剥离（Example 226）；collected 为逆序（末行在 car）
    (define (trim-last-paragraph-line collected)
      (if (null? collected)
        '()
        (cons (string-trim-right (car collected)) (cdr collected))
      ) ;if
    ) ;define

    ;; 返回 (list 段落行 rest underline-紧跟?)。underline 须紧跟段落末行（81/95）；
    ;; 空行后的 --- 是 hr 而非 underline（104）；空白行结束段落（227）
    (define (collect-paragraph-lines lines)
      (let loop
        ((remaining lines) (collected '()) (first-line? #t))
        (cond
         ((null? remaining) (list (reverse (trim-last-paragraph-line collected)) '() #f))
         ((blank-or-whitespace-line? (car remaining))
          (list (reverse (trim-last-paragraph-line collected)) (cdr remaining) #f)
         ) ;
         ((and (not first-line?) (setext-underline? (car remaining)))
          (list (reverse (trim-last-paragraph-line collected)) remaining #t)
         ) ;
         ((and (not first-line?) (block-start? remaining))
          (list (reverse (trim-last-paragraph-line collected)) remaining #f)
         ) ;
         (else
           (loop (cdr remaining)
             (cons (strip-line-leading-whitespace (car remaining)) collected)
             #f
           ) ;loop
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (collect-paragraph-or-setext-block remaining)
      (let* ((parsed (collect-paragraph-lines remaining))
             (para-lines (car parsed))
             (rest-lines (cadr parsed))
             (underline-ended? (caddr parsed))
            ) ;
        (if underline-ended?
          ;; 多行段落 + underline → setext 标题（Example 81/82/95）
          (let ((underline (car rest-lines)))
            (cons
              (make-markdown-block 'setext-heading
                (append para-lines (list underline))
                `((level . ,(setext-heading-level underline))
                  (content
                    . ,(string-join (map string-trim-right para-lines) "\n")))
              ) ;make-markdown-block
              (cdr rest-lines)
            ) ;cons
          ) ;let
          (cons (make-markdown-block 'paragraph para-lines '()) rest-lines)
        ) ;if
      ) ;let*
    ) ;define

    (define (collect-next-markdown-block lines)
      (let ((remaining (skip-leading-blank-lines lines)))
        (if (null? remaining)
          (cons #f '())
          (cond ((thematic-break-line? (car remaining))
                 (cons (make-markdown-block 'thematic-break '() '()) (cdr remaining))
                ) ;
                ((math-block-start-line? (car remaining))
                 (let ((parsed (collect-math-block-lines remaining)))
                   (cons (make-markdown-block 'math-block (car parsed) '()) (cdr parsed))
                 ) ;let
                ) ;
                ((html-block-start? (car remaining))
                 (let ((parsed (collect-html-block-lines remaining)))
                   (cons (make-markdown-block 'html-block (car parsed) '()) (cdr parsed))
                 ) ;let
                ) ;
                ((markdown-ordered-list-item? (car remaining))
                 (let* ((first-line (car remaining))
                        (start (ordered-list-start-number first-line))
                        (delimiter (ordered-list-delimiter first-line))
                        (parsed (collect-ordered-list-items remaining))
                        (item-lines-list (caar parsed))
                        (content-indents (cdar parsed))
                        (tightness (list-tightness item-lines-list content-indents))
                        (rest-lines (cdr parsed))
                       ) ;
                   (cons
                     (make-markdown-block 'ordered-list
                       item-lines-list
                       `((start . ,start)
                         (delimiter . ,delimiter)
                         (tightness . ,tightness))
                     ) ;make-markdown-block
                     rest-lines
                   ) ;cons
                 ) ;let*
                ) ;
                ((markdown-unordered-list-item? (car remaining))
                 (let* ((parsed (collect-bullet-list-items remaining))
                        (item-lines-list (caar parsed))
                        (content-indents (cdar parsed))
                        (tightness (list-tightness item-lines-list content-indents))
                        (rest-lines (cdr parsed))
                       ) ;
                   (cons
                     (make-markdown-block 'bullet-list item-lines-list `((tightness
                                                                           . ,tightness)))
                     rest-lines
                   ) ;cons
                 ) ;let*
                ) ;
                ((alert-block-start? (car remaining))
                 (let* ((parsed (collect-alert-block-lines remaining))
                        (alert-data (car parsed))
                        (alert-lines (cdr (assoc 'lines alert-data)))
                        (kind (cdr (assoc 'kind alert-data)))
                       ) ;
                   (cons
                     (make-markdown-block 'alert alert-lines `((kind . ,kind)))
                     (cdr parsed)
                   ) ;cons
                 ) ;let*
                ) ;
                ((blockquote-line? (car remaining))
                 (let ((parsed (collect-blockquote-lines-with-lazy remaining)))
                   (cons (make-markdown-block 'blockquote (car parsed) '()) (cdr parsed))
                 ) ;let
                ) ;
                ((fenced-code-start? (car remaining))
                 (let ((parsed (collect-fenced-code-block-lines remaining)))
                   (cons (make-markdown-block 'fenced-code-block (car parsed) '()) (cdr parsed))
                 ) ;let
                ) ;
                ((indented-code-line? (car remaining))
                 (let ((parsed (collect-indented-code-block-lines remaining)))
                   (cons (make-markdown-block 'indented-code-block (car parsed) '()) (cdr parsed))
                 ) ;let
                ) ;
                ((setext-heading-start? remaining)
                 (let* ((line (car remaining))
                        (underline (cadr remaining))
                        (level (setext-heading-level underline))
                        (content-line (string-trim (strip-line-leading-whitespace line)))
                       ) ;
                   (cons
                     (make-markdown-block 'setext-heading
                       (list line underline)
                       `((level . ,level) (content . ,content-line))
                     ) ;make-markdown-block
                     (cddr remaining)
                   ) ;cons
                 ) ;let*
                ) ;
                ((atx-heading-line? (car remaining))
                 (let* ((parsed-heading (parse-atx-heading-line (car remaining)))
                        (level (car parsed-heading))
                        (content (cdr parsed-heading))
                       ) ;
                   (cons
                     (make-markdown-block 'atx-heading
                       (list (car remaining))
                       `((level . ,level) (content . ,content))
                     ) ;make-markdown-block
                     (cdr remaining)
                   ) ;cons
                 ) ;let*
                ) ;
                ((table-start? remaining)
                 (let* ((parsed (collect-table-block remaining))
                        (table-data (car parsed))
                        (table-lines (cdr (assoc 'lines table-data)))
                       ) ;
                   (cons (make-markdown-block 'table table-lines table-data) (cdr parsed))
                 ) ;let*
                ) ;
                (else (collect-paragraph-or-setext-block remaining))
          ) ;cond
        ) ;if
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
