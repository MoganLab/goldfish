
(define-library (liii cmark block blockquote)
  (import (scheme base))
  (import (only (liii list) take-while drop-while))
  (import (liii cmark block line))
  (export blockquote-line? collect-blockquote-lines unquote-blockquote-line)
  (begin
    ;; 引用标记：前导空白 <= 3 的 >（Example 230）；4 空格及以上是缩进代码（Example 231）
    (define (blockquote-marker-index line)
      (let ((len (string-length line)))
        (let loop
          ((i 0))
          (cond ((>= i len) #f)
                ((and (< i 3) (char=? (string-ref line i) #\space)) (loop (+ i 1)))
                ((char=? (string-ref line i) #\>) i)
                (else #f)
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (blockquote-line? line)
      (if (blockquote-marker-index line) #t #f)
    ) ;define

    (define (collect-blockquote-lines lines)
      (cons (take-while blockquote-line? lines) (drop-while blockquote-line? lines))
    ) ;define

    ;; 剥掉前导空白 + > 标记（占 1 列），后跟至多 1 列可选空白（空格或 tab，Example 6）。
    ;; tab 可能被部分消费：消费 1 列后 tab 剩余列从新列展开为空格
    ;; （tab 宽 = 4 - (col mod 4)，从起始列展开结果与 cmark add_line 物化一致）。
    ;; 惰性续行（无 > 标记）原样返回。
    (define (unquote-blockquote-line line)
      (let ((quote-index (blockquote-marker-index line)))
        (if quote-index
          (let ((rest (substring line (+ quote-index 1))))
            (if
              (and (> (string-length rest) 0)
                (or (char=? (string-ref rest 0) #\space) (char=? (string-ref rest 0) #\tab))
              ) ;and
              (if (char=? (string-ref rest 0) #\space)
                ;; 空格：剥掉 1 个空格，剩余从起始列 2 展开前导 tab
                (expand-leading-tabs (substring rest 1) (+ quote-index 2))
                ;; tab：保留字符（部分消费 1 列），从起始列 2 展开剩余列
                (expand-leading-tabs rest (+ quote-index 2))
              ) ;if
              ;; 无可选空白 → 起始列 = quote-index + 1，展开剩余前导 tab
              (expand-leading-tabs rest (+ quote-index 1))
            ) ;if
          ) ;let
          line
        ) ;if
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
