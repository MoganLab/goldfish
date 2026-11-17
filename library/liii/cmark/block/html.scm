
(define-library (liii cmark block html)
  (import (scheme base))
  (import (scheme char))
  (import (liii cmark text))
  (import (liii cmark block line))
  (import (liii cmark common))
  (import (only (liii cmark inline html) find-inline-html-end))
  (export html-block-start?
    html-block-interrupts-paragraph?
    collect-html-block-lines
  ) ;export
  (begin
    (define *html-block-tag-names*
      '("address" "article" "aside" "base" "basefont" "blockquote" "body"
        "caption" "center" "col" "colgroup" "dd" "details" "dialog" "dir" "div"
        "dl" "dt" "fieldset" "figcaption" "figure" "footer" "form" "frame"
        "frameset" "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hr" "html"
        "iframe" "legend" "li" "link" "main" "menu" "menuitem" "nav" "noframes"
        "ol" "optgroup" "option" "p" "param" "search" "section" "summary"
        "table" "tbody" "td" "tfoot" "th" "thead" "title" "tr" "track" "ul")
    ) ;define

    (define *html-verbatim-tag-names* '("pre" "script" "style" "textarea"))

    (define (string-ci-contains? str pattern)
      (let ((len (string-length str)) (pattern-len (string-length pattern)))
        (let loop
          ((i 0))
          (cond ((> (+ i pattern-len) len) #f)
                ((let match-loop
                   ((j 0))
                   (cond ((>= j pattern-len) #t)
                         ((char-ci=? (string-ref str (+ i j)) (string-ref pattern j))
                          (match-loop (+ j 1))
                         ) ;
                         (else #f)
                   ) ;cond
                 ) ;let
                 i
                ) ;
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (html-whitespace-or-tag-end? ch)
      (or (char-whitespace? ch) (char=? ch #\>) (char=? ch #\/))
    ) ;define

    (define (normalize-tag-name name)
      (list->string (map char-downcase (string->list name)))
    ) ;define

    (define (only-whitespace-after? str start)
      (let loop
        ((i start))
        (cond ((>= i (string-length str)) #t)
              ((char-whitespace? (string-ref str i)) (loop (+ i 1)))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; 薄适配：标签名扫描在 cmark-utils 的 read-html-tag-name，此处补规范化与 (tag . end) 返回值
    (define (read-tag-name str start)
      (let ((name-end (read-html-tag-name str start)))
        (and name-end
          (cons (normalize-tag-name (substring str start name-end)) name-end)
        ) ;and
      ) ;let
    ) ;define

    (define (block-tag-name? name)
      (if (member name *html-block-tag-names* string=?) #t #f)
    ) ;define

    (define (html-block-tag-name? name)
      (or (block-tag-name? name) (verbatim-tag-name? name))
    ) ;define

    (define (verbatim-tag-name? name)
      (if (member name *html-verbatim-tag-names* string=?) #t #f)
    ) ;define

    (define (opening-block-tag-name trimmed)
      (and (> (string-length trimmed) 1)
        (char=? (string-ref trimmed 0) #\<)
        (let ((tag (read-tag-name trimmed 1)))
          (and tag
            (html-block-tag-name? (car tag))
            (or (= (cdr tag) (string-length trimmed))
              (html-whitespace-or-tag-end? (string-ref trimmed (cdr tag)))
            ) ;or
            (car tag)
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    (define (closing-block-tag-name trimmed)
      (and (> (string-length trimmed) 2)
        (string-prefix? "</" trimmed)
        (let ((tag (read-tag-name trimmed 2)))
          (and tag
            (html-block-tag-name? (car tag))
            (let loop
              ((i (cdr tag)))
              (cond ((>= i (string-length trimmed)) #f)
                    ((char-whitespace? (string-ref trimmed i)) (loop (+ i 1)))
                    ((char=? (string-ref trimmed i) #\>)
                     (and (only-whitespace-after? trimmed (+ i 1)) (car tag))
                    ) ;
                    (else #f)
              ) ;cond
            ) ;let
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    ;; 字符串以 `<` 开头且长度 > 1（HTML 标签候选）
    (define (tag-open? s len)
      (and (> len 1) (char=? (string-ref s 0) #\<))
    ) ;define

    (define (complete-tag-line-tag-name trimmed)
      (let ((len (string-length trimmed)) (end-pos (find-inline-html-end trimmed 0)))
        (and end-pos
          (only-whitespace-after? trimmed end-pos)
          (not (comment-start? trimmed 0))
          (not (processing-instruction-start? trimmed 0))
          (not (cdata-start? trimmed 0))
          (not (declaration-start? trimmed 0))
          (let ((tag
                  (cond ((and (> len 2) (string-prefix? "</" trimmed)) (read-tag-name trimmed 2))
                        ((tag-open? trimmed len) (read-tag-name trimmed 1))
                        (else #f)
                  ) ;cond
                ) ;tag
               ) ;
            (and tag
              (or (= (cdr tag) len)
                (html-whitespace-or-tag-end? (string-ref trimmed (cdr tag)))
              ) ;or
              (not (html-block-tag-name? (car tag)))
              (car tag)
            ) ;and
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (html-block-kind line)
      (let ((indent (count-leading-spaces line)))
        (and (<= indent 3)
          (let ((trimmed (substring line indent)))
            (cond ((comment-start? trimmed 0) 'comment)
                  ((processing-instruction-start? trimmed 0) 'processing-instruction)
                  ((cdata-start? trimmed 0) 'cdata)
                  ((declaration-start? trimmed 0) 'declaration)
                  ((opening-block-tag-name trimmed)
                   =>
                   (lambda (tag-name)
                     (if (verbatim-tag-name? tag-name)
                       (list 'verbatim tag-name)
                       (list 'block-tag tag-name)
                     ) ;if
                   ) ;lambda
                  ) ;
                  ((closing-block-tag-name trimmed)
                   =>
                   (lambda (tag-name) (list 'block-tag tag-name))
                  ) ;
                  ((complete-tag-line-tag-name trimmed)
                   =>
                   (lambda (tag-name) (list 'complete-tag tag-name))
                  ) ;
                  (else #f)
            ) ;cond
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (html-block-start? line)
      (if (html-block-kind line) #t #f)
    ) ;define

    (define (html-block-interrupts-paragraph? line)
      (let ((kind (html-block-kind line)))
        (and kind (not (and (pair? kind) (eq? (car kind) 'complete-tag))))
      ) ;let
    ) ;define

    (define (html-block-ended-by-line? kind line)
      (cond ((eq? kind 'comment) (if (string-ci-contains? line "-->") #t #f))
            ((eq? kind 'processing-instruction) (if (string-ci-contains? line "?>") #t #f))
            ((eq? kind 'cdata) (if (string-ci-contains? line "]]>") #t #f))
            ((eq? kind 'declaration) (if (string-ci-contains? line ">") #t #f))
            ((and (pair? kind) (eq? (car kind) 'verbatim))
             (if (string-ci-contains? line (string-append "</" (cadr kind) ">")) #t #f)
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (collect-html-block-lines lines)
      (let ((kind (html-block-kind (car lines))))
        (let loop
          ((remaining lines) (collected '()) (first-line? #t))
          (cond ((null? remaining) (cons (reverse collected) '()))
                ((and (pair? kind)
                   (or (eq? (car kind) 'block-tag) (eq? (car kind) 'complete-tag))
                   (not first-line?)
                   (blank-or-whitespace-line? (car remaining))
                 ) ;and
                 (cons (reverse collected) remaining)
                ) ;
                (else
                  (let ((line (car remaining)) (rest (cdr remaining)))
                    (if (html-block-ended-by-line? kind line)
                      (cons (reverse (cons line collected)) rest)
                      (loop rest (cons line collected) #f)
                    ) ;if
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
