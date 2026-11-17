(define-library (liii cmark block heading-code)
  (import (scheme base))
  (import (scheme char))
  (import (only (liii list) drop-while))
  (import (only (liii string) string-every string-skip-right))
  (import (liii cmark text))
  (import (liii cmark block line))
  (export first-word parse-fence-line fenced-code-start?
    collect-fenced-code-block-lines fenced-code-block-end? indented-code-line?
    strip-code-indent collect-indented-code-block-lines setext-underline?
    setext-heading-level setext-heading-start? parse-atx-heading-line
    atx-heading-line?
  ) ;export
  (begin
    (define (first-word str)
      (if (string=? str "")
        ""
        (let ((sp (string-index str #\space 0)))
          (if sp (substring str 0 sp) str)
        ) ;let
      ) ;if
    ) ;define

    ;; 解析围栏行 → (list 字符 长度 缩进 info) 或 #f。
    ;; 反引号围栏的 info 不能含反引号（Example 138/145）；info 取首个单词（143）
    (define (parse-fence-line line)
      (let* ((len (string-length line)) (indent (count-leading-spaces line)))
        (and (<= indent 3)
          (< indent len)
          (let ((c (string-ref line indent)))
            (and (or (char=? c #\`) (char=? c #\~))
              (let* ((fence-len (char-run-length line indent c))
                     (raw-info (string-trim (substring line (+ indent fence-len) len)))
                    ) ;
                (and (>= fence-len 3)
                  (or (char=? c #\~) (not (string-index raw-info #\` 0)))
                  (list c fence-len indent (first-word raw-info))
                ) ;and
              ) ;let*
            ) ;and
          ) ;let
        ) ;and
      ) ;let*
    ) ;define

    (define (fenced-code-start? line)
      (if (parse-fence-line line) #t #f)
    ) ;define

    (define (collect-fenced-code-block-lines lines)
      (let* ((fence (parse-fence-line (car lines)))
             (fence-char (car fence))
             (fence-len (cadr fence))
            ) ;
        (let loop
          ((remaining (cdr lines)) (collected (list (car lines))))
          (cond ((null? remaining) (cons (reverse collected) '()))
                ((fenced-code-block-end? fence-char fence-len (car remaining))
                 (cons (reverse (cons (car remaining) collected)) (cdr remaining))
                ) ;
                (else (loop (cdr remaining) (cons (car remaining) collected)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    ;; 闭合行: 前导空白 <= 3，同字符 run 长度 >= fence-len，其余全为空白
    (define (fenced-code-block-end? fence-char fence-len line)
      (let* ((len (string-length line)) (indent (count-leading-spaces line)))
        (and (<= indent 3)
          (< indent len)
          (char=? (string-ref line indent) fence-char)
          (let ((run-len (char-run-length line indent fence-char)))
            (and (>= run-len fence-len)
              (= (skip-ascii-whitespace line (+ indent run-len)) len)
            ) ;and
          ) ;let
        ) ;and
      ) ;let*
    ) ;define

    (define (indented-code-line? line)
      (and (> (string-length line) 0)
        (or (char=? (string-ref line 0) #\tab)
          (and (>= (string-length line) 4)
            (char=? (string-ref line 0) #\space)
            (char=? (string-ref line 1) #\space)
            (char=? (string-ref line 2) #\space)
            (char=? (string-ref line 3) #\space)
          ) ;and
        ) ;or
      ) ;and
    ) ;define

    ;; 剥掉 4 空格或 1 个 tab
    (define (unindent-code-line line)
      (if (and (> (string-length line) 0) (char=? (string-ref line 0) #\tab))
        (substring line 1)
        (substring line 4)
      ) ;if
    ) ;define

    ;; 内容行剥 min(行缩进, 围栏缩进)（Example 131-133）
    (define (strip-code-indent indent line)
      (let ((n (min indent (count-leading-spaces line))))
        (substring line n)
      ) ;let
    ) ;define

    ;; 缩进代码块跨空白行连续（85/110/111）：空行后首个非空行仍是缩进行时空白行并入块内
    (define (collect-indented-code-block-lines lines)
      (let loop
        ((remaining (trim-leading-blank-or-whitespace-lines lines)) (collected '()))
        (cond ((null? remaining) (cons (reverse collected) '()))
              ((indented-code-line? (car remaining))
               (loop (cdr remaining) (cons (unindent-code-line (car remaining)) collected))
              ) ;
              ((blank-or-whitespace-line? (car remaining))
               (let ((trimmed (trim-leading-blank-or-whitespace-lines (cdr remaining))))
                 (if (and (pair? trimmed) (indented-code-line? (car trimmed)))
                   (loop (cdr remaining) (cons "" collected))
                   (cons (reverse collected) remaining)
                 ) ;if
               ) ;let
              ) ;
              (else (cons (reverse collected) remaining))
        ) ;cond
      ) ;let
    ) ;define

    (define (trim-leading-blank-or-whitespace-lines lines)
      (drop-while blank-or-whitespace-line? lines)
    ) ;define

    ;; underline: 前导空白 <= 3，去空白后 >= 1 个连续 = 或 -（CommonMark：至少 1 个即可，如 Example 83 的 `Foo\n=`）
    (define (setext-underline? line)
      (and (<= (count-leading-spaces line) 3)
        (let ((trimmed (string-trim line)))
          (and (> (string-length trimmed) 0)
            (let ((first-char (string-ref trimmed 0)))
              (and (or (char=? first-char #\=) (char=? first-char #\-))
                (string-every first-char trimmed)
              ) ;and
            ) ;let
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    (define (setext-heading-level line)
      (let ((trimmed (string-trim line)))
        (if (and (> (string-length trimmed) 0) (char=? (string-ref trimmed 0) #\=)) 1 2)
      ) ;let
    ) ;define

    (define (setext-heading-start? lines)
      (and (pair? lines)
        (pair? (cdr lines))
        (not (blank-line? (car lines)))
        (setext-underline? (cadr lines))
      ) ;and
    ) ;define

    (define (heading-marker-terminated? line hash-count marker-start)
      (let ((len (string-length line)))
        (or (= (+ marker-start hash-count) len)
          (char-whitespace? (string-ref line (+ marker-start hash-count)))
        ) ;or
      ) ;let
    ) ;define

    ;; 标题内容：去掉 marker 后空白，保留结尾 # 序列
    (define (normalize-atx-content line hash-count marker-start)
      (let* ((len (string-length line))
             (start (skip-ascii-whitespace line (+ marker-start hash-count)))
             (content (if (>= start len) "" (substring line start len)))
            ) ;
        (strip-atx-closing-sequence content)
      ) ;let*
    ) ;define

    ;; 保留结尾 # 序列（Example 71/72/73/79）：# 序列前空白压缩为单个空格
    (define (strip-atx-closing-sequence content)
      (let* ((trimmed (string-trim-right content))
             (len (string-length trimmed))
             (i (string-skip-right trimmed #\#))
            ) ;
        (if (and i (ascii-whitespace? (string-ref trimmed i)))
          (let ((j (string-skip-right trimmed ascii-whitespace? 0 i)))
            (string-append (substring trimmed 0 (if j (+ j 1) 0))
              " "
              (substring trimmed (+ i 1) len)
            ) ;string-append
          ) ;let
          trimmed
        ) ;if
      ) ;let*
    ) ;define

    ;; 前导空白 <= 3 仍识别为标题（Example 68）；4 空格及以上不是标题（Example 70）
    (define (parse-atx-heading-line line)
      (let* ((len (string-length line)) (marker-start (skip-ascii-whitespace line 0)))
        (and (<= marker-start 3)
          (let ((hash-count (char-run-length line marker-start #\#)))
            (and (> hash-count 0)
              (<= hash-count 6)
              (heading-marker-terminated? line hash-count marker-start)
              (cons hash-count (normalize-atx-content line hash-count marker-start))
            ) ;and
          ) ;let
        ) ;and
      ) ;let*
    ) ;define

    (define (atx-heading-line? line)
      (if (parse-atx-heading-line line) #t #f)
    ) ;define

    ;; 列表项中断段落：空 item 不能中断（Example 285）；有序起始数必须为 1（Example 304）
  ) ;begin
) ;define-library
