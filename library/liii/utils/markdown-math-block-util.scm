
(define-library (liii utils markdown-math-block-util)
  (import (scheme base))
  (import (liii utils text-util))
  (export math-block-start-line?)
  (export collect-math-block-lines)
  (begin
    (define (trimmed-block-line line)
      (let ((indent (count-leading-spaces line)))
        (and (<= indent 3) (string-trim (substring line indent)))
      ) ;let
    ) ;define

    (define (fenced-math-fence trimmed)
      (cond
       ((and (string-prefix? "```" trimmed)
          (string=? (string-trim (substring trimmed 3)) "math")
        ) ;and
        "```"
       ) ;
       ((and (string-prefix? "~~~" trimmed)
          (string=? (string-trim (substring trimmed 3)) "math")
        ) ;and
        "~~~"
       ) ;
       (else #f)
      ) ;cond
    ) ;define

    ;; \[...\] 单行块：以 \[ 开头、以 \] 结尾、定界间至少一个字符
    ;; （与 cmark/math-parser.scm 中的同名谓词保持同步）
    (define (single-line-bracket-math? trimmed)
      (let ((len (string-length trimmed)))
        (and (> len 3)
          (string-prefix? "\\[" trimmed)
          (char=? (string-ref trimmed (- len 2)) #\\)
          (char=? (string-ref trimmed (- len 1)) #\])
        ) ;and
      ) ;let
    ) ;define

    ;; $$...$$ 单行块：以 $$ 开头、以 $$ 结尾、定界间至少一个字符
    ;; （与 cmark/math-parser.scm 中的同名谓词保持同步）
    (define (single-line-dollar-math? trimmed)
      (let ((len (string-length trimmed)))
        (and (> len 4)
          (string-prefix? "$$" trimmed)
          (char=? (string-ref trimmed (- len 2)) #\$)
          (char=? (string-ref trimmed (- len 1)) #\$)
        ) ;and
      ) ;let
    ) ;define

    ;; 块级 math 定界：$$（含单行 $$...$$）、```math/~~~math 围栏、
    ;; \[（多行起始或单行 \[...\]）
    ;; 仅行首识别：段落中部的 \[...\] 不构成块（Math43/44）
    (define (math-block-start-line? line)
      (let ((trimmed (trimmed-block-line line)))
        (and trimmed
          (or (string=? trimmed "$$")
            (fenced-math-fence trimmed)
            (string=? trimmed "\\[")
            (single-line-bracket-math? trimmed)
            (single-line-dollar-math? trimmed)
          ) ;or
        ) ;and
      ) ;let
    ) ;define

    (define (math-delimiter-end-line? start-line line)
      (let ((start-trimmed (trimmed-block-line start-line))
            (end-trimmed (trimmed-block-line line))
           ) ;
        (and end-trimmed
          (cond ((string=? start-trimmed "$$") (string=? end-trimmed "$$"))
                ((string=? start-trimmed "\\[") (string=? end-trimmed "\\]"))
                (else #f)
          ) ;cond
        ) ;and
      ) ;let
    ) ;define

    (define (fenced-math-end-line? fence line)
      (let ((trimmed (trimmed-block-line line)))
        (and trimmed
          (string-prefix? fence trimmed)
          (string=? (string-trim (substring trimmed (string-length fence))) "")
        ) ;and
      ) ;let
    ) ;define

    (define (collect-math-block-lines lines)
      (let* ((first-line (car lines))
             (trimmed (trimmed-block-line first-line))
             (fence (and trimmed (fenced-math-fence trimmed)))
            ) ;
        ;; 单行 \[...\] / $$...$$ 首尾同行，直接返回；否则逐行收集到结束定界
        (if (and trimmed
              (or (single-line-bracket-math? trimmed) (single-line-dollar-math? trimmed))
            ) ;and
          (cons (list first-line) (cdr lines))
          (let loop
            ((remaining (cdr lines)) (collected (list first-line)))
            (cond ((null? remaining) (cons (reverse collected) '()))
                  ((and fence (fenced-math-end-line? fence (car remaining)))
                   (cons (reverse (cons (car remaining) collected)) (cdr remaining))
                  ) ;
                  ((and (not fence) (math-delimiter-end-line? first-line (car remaining)))
                   (cons (reverse (cons (car remaining) collected)) (cdr remaining))
                  ) ;
                  (else (loop (cdr remaining) (cons (car remaining) collected)))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library
