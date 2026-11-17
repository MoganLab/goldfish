
(define-library (liii cmark block list)
  (import (scheme base))
  (import (scheme char))
  (import (liii cmark text))
  (import (liii cmark block line))
  (export make-markdown-list-marker
    markdown-list-marker?
    markdown-list-marker-indent
    markdown-list-marker-type
    markdown-list-marker-start
    markdown-list-marker-delimiter
    markdown-list-marker-content-indent
    markdown-list-marker-body
    parse-list-marker
    parse-markdown-list-marker
    unordered-list-item?
    ordered-list-item?
    ordered-list-delimiter
    ordered-list-start-number
    strip-ordered-list-marker
    parse-list-item
    markdown-list-item-indent
    strip-markdown-list-indent
    markdown-unordered-list-item?
    markdown-ordered-list-item?
    list-item-marker-line?
    empty-list-item?
    list-item-content-indent
    strip-bullet-list-marker

    task-list-item-status
    strip-task-list-item-marker
    same-list-item?
  ) ;export
  (begin
    (define-record-type <markdown-list-marker>
      (make-markdown-list-marker indent type start delimiter content-indent body)
      markdown-list-marker?
      (indent markdown-list-marker-indent)
      (type markdown-list-marker-type)
      (start markdown-list-marker-start)
      (delimiter markdown-list-marker-delimiter)
      (content-indent markdown-list-marker-content-indent)
      (body markdown-list-marker-body)
    ) ;define-record-type

    (define (bullet-marker-char? ch)
      (or (char=? ch #\-) (char=? ch #\*) (char=? ch #\+))
    ) ;define

    ;; 有序标记：1-9 位数字 + . 或 )，后跟空白或行尾（行尾是空 item，Example 283）
    (define (ordered-list-marker-index line)
      (let ((len (string-length line)))
        (let loop
          ((i 0))
          (cond ((>= i len) #f)
                ((and (< i 9) (char-numeric? (string-ref line i))) (loop (+ i 1)))
                ((and (> i 0)
                   (or (char=? (string-ref line i) #\.) (char=? (string-ref line i) #\)))
                   (or (= (+ i 1) len) (char-whitespace? (string-ref line (+ i 1))))
                 ) ;and
                 i
                ) ;
                (else #f)
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    ;; marker 后的内容 body 按列展开前导 tab（Example 7）。
    ;; 模拟 cmark list marker padding（blocks.c open_new_blocks）：
    ;; marker 后前导空白列数 i = 展开后的空格数。
    ;; - 全空白（空 item）→ body 为空
    ;; - i >= 5：padding = matched + 1，只消费 1 列
    ;;   （空格剥掉；tab 部分消费保留，从新列展开剩余列，与 add_line 物化一致）。
    ;; - 1 <= i < 5：padding = matched + i，前导空白全部消费。
    (define (list-marker-body line indent marker-char-count)
      (let* ((after-marker (substring line marker-char-count))
             (base-col (+ indent marker-char-count))
             (expanded (expand-leading-tabs after-marker base-col))
             (lead (count-leading-spaces expanded))
             (rest (substring expanded lead))
            ) ;
        (cond ((string=? (string-trim rest) "") "")
              ((>= lead 5)
               (if (and (> (string-length after-marker) 0)
                     (char=? (string-ref after-marker 0) #\space)
                   ) ;and
                 (expand-leading-tabs (substring after-marker 1) (+ base-col 1))
                 (expand-leading-tabs after-marker (+ base-col 1))
               ) ;if
              ) ;
              (else rest)
        ) ;cond
      ) ;let*
    ) ;define

    (define (parse-list-marker-core line indent)
      (cond
       ((and (>= (string-length line) 1)
          (bullet-marker-char? (string-ref line 0))
          (or (= (string-length line) 1) (char-whitespace? (string-ref line 1)))
        ) ;and
        (make-markdown-list-marker indent
          'bullet
          1
          (string-ref line 0)
          (list-marker-content-indent line indent 1)
          (list-marker-body line indent 1)
        ) ;make-markdown-list-marker
       ) ;
       (else
         (let ((marker-index (ordered-list-marker-index line)))
           (if marker-index
             (let* ((delimiter-char (string-ref line marker-index))
                    (start (string->number (substring line 0 marker-index)))
                    (marker-char-count (+ marker-index 1))
                   ) ;
               (make-markdown-list-marker indent
                 'ordered
                 start
                 (if (char=? delimiter-char #\.) 'period 'one-paren)
                 (list-marker-content-indent line indent marker-char-count)
                 (list-marker-body line indent marker-char-count)
               ) ;make-markdown-list-marker
             ) ;let*
             #f
           ) ;if
         ) ;let
       ) ;else
      ) ;cond
    ) ;define

    ;; content-indent（规范 W+N / W+1）：
    ;; marker 后 1-4 空格 → marker 宽 + 空格数（首个非空白列）；
    ;; 5+ 空格或空 item（marker 后全空白/行尾）→ marker 宽 + 1。
    (define (list-marker-content-indent line indent marker-char-count)
      (let* ((after-marker (substring line marker-char-count))
             (base-col (+ indent marker-char-count))
             (expanded (expand-leading-tabs after-marker base-col))
             (lead (count-leading-spaces expanded))
             (rest (substring expanded lead))
            ) ;
        (if
          (or (>= lead 5) (and (>= lead 1) (string=? (string-trim rest) "")) (= lead 0))
          (+ base-col 1)
          (+ base-col lead)
        ) ;if
      ) ;let*
    ) ;define

    (define (parse-list-marker line)
      (parse-list-marker-core line 0)
    ) ;define

    (define (parse-markdown-list-marker line)
      (let ((indent (count-leading-spaces line)))
        (if (and (<= indent 3) (< indent (string-length line)))
          (parse-list-marker-core (substring line indent) indent)
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (unordered-list-item? line)
      (let ((marker (parse-list-marker line)))
        (and marker (eq? (markdown-list-marker-type marker) 'bullet))
      ) ;let
    ) ;define

    (define (ordered-list-item? line)
      (let ((marker (parse-list-marker line)))
        (and marker (eq? (markdown-list-marker-type marker) 'ordered))
      ) ;let
    ) ;define

    (define (ordered-list-delimiter line)
      (let ((marker (parse-markdown-list-marker line)))
        (if (and marker (eq? (markdown-list-marker-type marker) 'ordered))
          (markdown-list-marker-delimiter marker)
          'period
        ) ;if
      ) ;let
    ) ;define

    (define (ordered-list-start-number line)
      (let ((marker (parse-markdown-list-marker line)))
        (if (and marker (eq? (markdown-list-marker-type marker) 'ordered))
          (markdown-list-marker-start marker)
          1
        ) ;if
      ) ;let
    ) ;define

    (define (strip-ordered-list-marker line)
      (let ((marker (parse-list-marker line)))
        (if marker (markdown-list-marker-body marker) line)
      ) ;let
    ) ;define

    (define (parse-list-item line)
      (let ((marker (parse-list-marker line)))
        (cond ((not marker) (cons 'bullet line))
              ((eq? (markdown-list-marker-type marker) 'bullet)
               (cons 'bullet (markdown-list-marker-body marker))
              ) ;
              (else (cons (cons 'ordered (markdown-list-marker-start marker))
                      (markdown-list-marker-body marker)
                    ) ;cons
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (markdown-list-item-indent line)
      (let ((marker (parse-markdown-list-marker line)))
        (and marker (markdown-list-marker-indent marker))
      ) ;let
    ) ;define

    (define (strip-markdown-list-indent line)
      (let ((marker (parse-markdown-list-marker line)))
        (if marker (substring line (markdown-list-marker-indent marker)) line)
      ) ;let
    ) ;define

    (define (markdown-unordered-list-item? line)
      (let ((marker (parse-markdown-list-marker line)))
        (and marker (eq? (markdown-list-marker-type marker) 'bullet))
      ) ;let
    ) ;define

    (define (markdown-ordered-list-item? line)
      (let ((marker (parse-markdown-list-marker line)))
        (and marker (eq? (markdown-list-marker-type marker) 'ordered))
      ) ;let
    ) ;define

    (define (list-item-marker-line? line)
      (if (parse-markdown-list-marker line) #t #f)
    ) ;define

    ;; 空 item（marker 后无内容）不能中断段落（Example 285）
    (define (empty-list-item? line)
      (let ((marker (parse-markdown-list-marker line)))
        (and marker (string=? (markdown-list-marker-body marker) ""))
      ) ;let
    ) ;define

    ;; 同一列表判定：同类型 + 同定界符/字符，且缩进 < 前项 content-indent
    ;; （Example 294 缩进 = content-indent → 嵌套；295/310/311 缩进 < → 同级）
    (define (same-list-item? line prev-marker)
      (let ((marker (parse-markdown-list-marker line)))
        (and marker
          (< (markdown-list-marker-indent marker)
            (markdown-list-marker-content-indent prev-marker)
          ) ;<
          (eq? (markdown-list-marker-type marker) (markdown-list-marker-type prev-marker))
          (eq? (markdown-list-marker-delimiter marker)
            (markdown-list-marker-delimiter prev-marker)
          ) ;eq?
        ) ;and
      ) ;let
    ) ;define

    (define (list-item-content-indent line)
      (let ((marker (parse-markdown-list-marker line)))
        (if marker (markdown-list-marker-content-indent marker) 0)
      ) ;let
    ) ;define

    (define (strip-bullet-list-marker line)
      (let ((marker (parse-markdown-list-marker line)))
        (if (and marker (eq? (markdown-list-marker-type marker) 'bullet))
          (markdown-list-marker-body marker)
          ""
        ) ;if
      ) ;let
    ) ;define

    (define (task-list-item-status line)
      (and (>= (string-length line) 3)
        (char=? (string-ref line 0) #\[)
        (let ((marker-char (string-ref line 1)))
          (and (or (char=? marker-char #\space)
                 (char=? marker-char #\x)
                 (char=? marker-char #\X)
               ) ;or
            (char=? (string-ref line 2) #\])
            (or (= (string-length line) 3) (char-whitespace? (string-ref line 3)))
            (if (char=? marker-char #\space) 'unchecked 'checked)
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    (define (strip-task-list-item-marker line)
      (if (task-list-item-status line)
        (let loop
          ((i 3))
          (if (and (< i (string-length line)) (char-whitespace? (string-ref line i)))
            (loop (+ i 1))
            (substring line i)
          ) ;if
        ) ;let
        line
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
