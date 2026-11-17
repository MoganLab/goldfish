;;;
;;; 行级判定工具（markdown 块级解析共用）
;;;
;;; Copyright (C) 2026 Liii Network All Right Reserved
;;;

(define-library (liii utils markdown-line-util)
  (import (scheme base))
  (import (scheme char))
  (import (liii utils text-util))
  (export blank-line? blank-or-whitespace-line? thematic-break-line?
    expand-leading-tabs
  ) ;export
  (begin
    (define (blank-line? line)
      (string=? line "")
    ) ;define

    ;; CommonMark §2.1 Tabs: tab 仅在前导（参与块结构的）空白处按 4 列 tab stop 展开为空格；
    ;; 行内 tab 在代码/普通文本中保持字面（Example 1-3/8 代码块内 tab 原样）。
    ;; 实现：逐字符消费前导空格与 tab，tab 展开到下一个 4 的倍数列；遇到首个非空白字符停止。
    ;; start-col: 该行首个字符的虚拟列（容器前缀消费后，如 blockquote 剥 > 后从 col 2 展开）。
    ;; tab 宽度 = 4 - (column mod 4)，从起始列起算，与 cmark S_advance_offset 列追踪一致
    ;; （部分消费的 tab 剩余列数 = 其完整宽度，展开结果相同，Example 6/7）。
    (define (expand-leading-tabs line . maybe-start-col)
      (let ((start-col (if (null? maybe-start-col) 0 (car maybe-start-col))))
        (let ((len (string-length line)))
          (let loop
            ((pos 0) (column start-col) (parts '()))
            (cond ((>= pos len) (string-join (reverse parts) ""))
                  ((char=? (string-ref line pos) #\space)
                   (loop (+ pos 1) (+ column 1) (cons " " parts))
                  ) ;
                  ((char=? (string-ref line pos) #\tab)
                   (let ((spaces (- 4 (modulo column 4))))
                     (loop (+ pos 1) (+ column spaces) (cons (make-string spaces #\space) parts))
                   ) ;let
                  ) ;
                  (else (string-append (string-join (reverse parts) "") (substring line pos len)))
            ) ;cond
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    (define (blank-or-whitespace-line? line)
      (string=? (string-trim line) "")
    ) ;define

    ;; 分隔线: 一行 3 个及以上相同的 */-/_（可夹空白），前导空白 <= 3
    (define (thematic-break-line? line)
      (let ((indent (count-leading-spaces line)))
        (and (<= indent 3)
          (< indent (string-length line))
          (let ((trimmed (substring line indent)))
            (and (>= (string-length trimmed) 3)
              (let ((c (string-ref trimmed 0)))
                (and (or (char=? c #\-) (char=? c #\*) (char=? c #\_))
                  (let loop
                    ((i 1) (count 1))
                    (cond ((>= i (string-length trimmed)) (>= count 3))
                          ((char=? (string-ref trimmed i) c) (loop (+ i 1) (+ count 1)))
                          ((ascii-whitespace? (string-ref trimmed i)) (loop (+ i 1) count))
                          (else #f)
                    ) ;cond
                  ) ;let
                ) ;and
              ) ;let
            ) ;and
          ) ;let
        ) ;and
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
