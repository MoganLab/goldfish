
(define-library (liii utils markdown-table-util)
  (import (scheme base))
  (import (liii utils text-util))
  (import (liii utils markdown-line-util))
  (export table-start? collect-table-block)
  (begin
    (define (table-indent-trim line)
      (let ((indent (count-leading-spaces line)))
        (if (<= indent 3) (substring line indent) #f)
      ) ;let
    ) ;define

    ;; 字符串首字符是否为 `|`（表格行标记）
    (define (starts-with-bar? s)
      (and (> (string-length s) 0) (char=? (string-ref s 0) #\|))
    ) ;define

    (define (trim-table-outer-pipes line cells)
      (let* ((leading-pipe?
               (let ((trimmed (string-trim-left line)))
                 (starts-with-bar? trimmed)
               ) ;let
             ) ;leading-pipe?
             (trailing-pipe?
               (let ((trimmed (string-trim-right line)))
                 (and (> (string-length trimmed) 0)
                   (char=? (string-ref trimmed (- (string-length trimmed) 1)) #\|)
                 ) ;and
               ) ;let
             ) ;trailing-pipe?
             (cells-without-leading (if (and leading-pipe? (pair? cells)) (cdr cells) cells))
            ) ;
        (if (and trailing-pipe? (pair? cells-without-leading))
          (reverse (cdr (reverse cells-without-leading)))
          cells-without-leading
        ) ;if
      ) ;let*
    ) ;define

    ;; GFM: 行内按未转义 | 切分单元格，code span 不保护管道（\| 转义仍有效）；
    ;; 超出表头列数的单元格由 normalize-table-row 丢弃。
    (define (split-table-line line)
      (let ((trimmed-line (table-indent-trim line)))
        (and trimmed-line
          (let ((len (string-length trimmed-line)))
            (let loop
              ((i 0) (cell-start 0) (cells '()) (saw-pipe? #f))
              (cond
               ((>= i len)
                (cons saw-pipe?
                  (trim-table-outer-pipes trimmed-line
                    (reverse (cons (substring trimmed-line cell-start len) cells))
                  ) ;trim-table-outer-pipes
                ) ;cons
               ) ;
               ((and (char=? (string-ref trimmed-line i) #\\) (< (+ i 1) len))
                (loop (+ i 2) cell-start cells saw-pipe?)
               ) ;
               ((char=? (string-ref trimmed-line i) #\|)
                (loop (+ i 1) (+ i 1) (cons (substring trimmed-line cell-start i) cells) #t)
               ) ;
               (else (loop (+ i 1) cell-start cells saw-pipe?))
              ) ;cond
            ) ;let
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (parse-table-row line)
      (let ((parsed (split-table-line line)))
        (and parsed (car parsed) (map string-trim (cdr parsed)))
      ) ;let
    ) ;define

    (define (alignment-cell-symbol cell)
      (let* ((trimmed (string-trim cell)) (len (string-length trimmed)))
        (and (> len 0)
          (let* ((left-colon? (char=? (string-ref trimmed 0) #\:))
                 (right-colon? (char=? (string-ref trimmed (- len 1)) #\:))
                 (start (if left-colon? 1 0))
                 (end (if right-colon? (- len 1) len))
                 (core (if (<= start end) (substring trimmed start end) ""))
                 (core-len (string-length core))
                ) ;
            (and (> core-len 0)
              (let loop
                ((i 0))
                (cond
                 ((>= i core-len)
                  (cond ((and left-colon? right-colon?) 'center)
                        (left-colon? 'left)
                        (right-colon? 'right)
                        (else 'default)
                  ) ;cond
                 ) ;
                 ((char=? (string-ref core i) #\-) (loop (+ i 1)))
                 (else #f)
                ) ;cond
              ) ;let
            ) ;and
          ) ;let*
        ) ;and
      ) ;let*
    ) ;define

    (define (parse-table-alignments line)
      (let ((cells (parse-table-row line)))
        (and cells
          (let loop
            ((remaining cells) (alignments '()))
            (cond ((null? remaining) (reverse alignments))
                  (else
                    (let ((alignment (alignment-cell-symbol (car remaining))))
                      (and alignment (loop (cdr remaining) (cons alignment alignments)))
                    ) ;let
                  ) ;else
            ) ;cond
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (table-start? lines)
      (and (pair? lines)
        (pair? (cdr lines))
        (let ((header (parse-table-row (car lines)))
              (alignments (parse-table-alignments (cadr lines)))
             ) ;
          (and header alignments (= (length header) (length alignments)))
        ) ;let
      ) ;and
    ) ;define

    (define (normalize-table-row cells column-count)
      (let ((current-count (length cells)))
        (cond ((= current-count column-count) cells)
              ((< current-count column-count)
               (append cells (make-empty-cells (- column-count current-count)))
              ) ;
              (else (take-cells cells column-count))
        ) ;cond
      ) ;let
    ) ;define

    (define (make-empty-cells count)
      (if (<= count 0) '() (cons "" (make-empty-cells (- count 1))))
    ) ;define

    (define (take-cells cells count)
      (if (or (<= count 0) (null? cells))
        '()
        (cons (car cells) (take-cells (cdr cells) (- count 1)))
      ) ;if
    ) ;define

    (define (collect-table-block lines)
      (let* ((header-line (car lines))
             (delimiter-line (cadr lines))
             (header (parse-table-row header-line))
             (alignments (parse-table-alignments delimiter-line))
             (column-count (length alignments))
            ) ;
        (let loop
          ((remaining (cddr lines))
           (raw-lines (list delimiter-line header-line))
           (rows '())
          ) ;
          (let ((next-row
                  (and (pair? remaining)
                    (not (blank-or-whitespace-line? (car remaining)))
                    (parse-table-row (car remaining))
                  ) ;and
                ) ;next-row
               ) ;
            (if next-row
              (loop (cdr remaining)
                (cons (car remaining) raw-lines)
                (cons (normalize-table-row next-row column-count) rows)
              ) ;loop
              (cons `((lines . ,(reverse raw-lines))
                      (alignments . ,alignments)
                      (header . ,(normalize-table-row header column-count))
                      (rows . ,(reverse rows)))
                remaining
              ) ;cons
            ) ;if
          ) ;let
        ) ;let
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library
