(define-library (liii cmark inline emphasis)
  (import (scheme base))
  (import (scheme char))
  (import (liii cmark inline char))
  (export left-flanking? right-flanking? marker-run-len intraword-run?
    emph-start? strong-start? backslash-escaped-at? find-marker-run-start
  ) ;export
  (begin
    (define (left-flanking? str pos run-len)
      (let ((len (string-length str)))
        (and (< (+ pos run-len) len)
          (not (unicode-whitespace-at? str (+ pos run-len)))
          (or (not (unicode-punctuation-at? str (+ pos run-len)))
            (or (= pos 0)
              (unicode-whitespace-before? str pos)
              (unicode-punctuation-before? str pos)
            ) ;or
          ) ;or
        ) ;and
      ) ;let
    ) ;define

    ;; 右翼（right-flanking）: 前非空白，且（前非标点 或 后是空白/标点）
    ;; 行首/行尾计为空白（CommonMark §6.2）；判定基于整个 delimiter run
    (define (right-flanking? str pos run-len)
      (let ((len (string-length str)))
        (and (> pos 0)
          (not (unicode-whitespace-before? str pos))
          (or (not (unicode-punctuation-before? str pos))
            (or (>= (+ pos run-len) len)
              (let ((next-start (+ pos run-len)))
                (or (unicode-whitespace-at? str next-start)
                  (unicode-punctuation-at? str next-start)
                ) ;or
              ) ;let
            ) ;or
          ) ;or
        ) ;and
      ) ;let
    ) ;define

    ;; 连续相同 marker（* 或 _）run 长度
    (define (marker-run-len str pos)
      (let ((len (string-length str)) (marker (string-ref str pos)))
        (let loop
          ((p pos) (n 0))
          (if (and (< p len)
                (char=? (string-ref str p) marker)
                (not (backslash-escaped-at? str p))
              ) ;and
            (loop (+ p 1) (+ n 1))
            n
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; _ run 整体 intraword 判定：前后均非空白且非标点（Example 386-388；空格/标点两侧不算 intraword）
    (define (intraword-run? str pos run-len)
      (and (> pos 0)
        (< (+ pos run-len) (string-length str))
        (let ((prev-start (previous-char-start str pos)) (next-start (+ pos run-len)))
          (and (>= prev-start 0)
            (not (unicode-whitespace-at? str prev-start))
            (not (unicode-punctuation-at? str prev-start))
            (not (unicode-whitespace-at? str next-start))
            (not (unicode-punctuation-at? str next-start))
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    ;; emph 匹配奇数 run（1/3/5...）：*** 先拆单 * 作外层 em，内层 ** 递归 strong（Example 413/416）
    (define (emph-start? str pos)
      (and (< pos (string-length str))
        (or (char=? (string-ref str pos) #\*) (char=? (string-ref str pos) #\_))
        ;; 仅 run 开头
        (or (= pos 0) (not (char=? (string-ref str (- pos 1)) (string-ref str pos))))
        (let ((run-len (marker-run-len str pos)))
          (and (odd? run-len)
            (left-flanking? str pos run-len)
            (or (char=? (string-ref str pos) #\*) (not (intraword-run? str pos run-len)))
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    ;; strong 匹配偶数 run（2/4/6...）：**** = strong(strong(...))（Example 417）
    (define (strong-start? str pos)
      (and (< (+ pos 1) (string-length str))
        (or (char=? (string-ref str pos) #\*) (char=? (string-ref str pos) #\_))
        (or (= pos 0) (not (char=? (string-ref str (- pos 1)) (string-ref str pos))))
        (let ((run-len (marker-run-len str pos)))
          (and (even? run-len)
            (>= run-len 2)
            (left-flanking? str pos run-len)
            (or (char=? (string-ref str pos) #\*) (not (intraword-run? str pos run-len)))
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    ;; pos 处字符是否被反斜杠转义（前一个字符是 \ 且该 \ 未被转义，Example 437/440/449/452）
    (define (backslash-escaped-at? str pos)
      (and (> pos 0)
        (char=? (string-ref str (- pos 1)) #\\)
        (or (< (- pos 1) 1) (not (char=? (string-ref str (- pos 2)) #\\)))
      ) ;and
    ) ;define

    ;; 从 pos 回退到同 marker（* 或 _）run 的开头
    (define (find-marker-run-start str pos)
      (let ((marker (string-ref str pos)))
        (let loop
          ((p pos))
          (if
            (and (> p 0)
              (char=? (string-ref str (- p 1)) marker)
              (not (backslash-escaped-at? str (- p 1)))
            ) ;and
            (loop (- p 1))
            p
          ) ;if
        ) ;let
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
