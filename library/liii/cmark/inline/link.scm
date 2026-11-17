
(define-library (liii cmark inline link)
  (import (scheme base))
  (import (liii string))
  (import (liii cmark inline entity))
  (import (liii cmark inline escape))
  (import (liii cmark common))
  (import (liii cmark text))
  (import (liii cmark inline html))
  (import (liii cmark inline autolink))
  (export string-find-char find-matching-bracket parse-link-destination
    parse-inline-link-rest skip-html-or-autolink code-span-end
  ) ;export
  (begin
    ;; string-find-char 复用 (liii string) 的 string-index（签名一致）
    (define (string-find-char str char start-pos)
      (string-index str char start-pos)
    ) ;define

    (define (count-backtick-run str pos len)
      (let scan
        ((p pos) (n 0))
        (if (char-at? str p len #\`) (scan (+ p 1) (+ n 1)) n)
      ) ;let
    ) ;define

    ;; code span 不参与 emphasis/strong/link 的 delimiter 匹配（CommonMark §6.3）
    ;; 若 str 在 pos 处是 backtick run 且其后存在等长 run，返回 code span 结束位置（不含闭合 run）；否则 #f
    (define (code-span-end str pos)
      (let ((len (string-length str)))
        (and (< pos len)
          (char=? (string-ref str pos) #\`)
          (let* ((marker-len (count-backtick-run str pos len))
                 (search-start (+ pos marker-len))
                ) ;
            (let loop
              ((p search-start))
              (cond ((>= p len) #f)
                    ((char=? (string-ref str p) #\`)
                     (let ((run-len (count-backtick-run str p len)))
                       (if (= run-len marker-len) (+ p run-len) (loop (+ p run-len)))
                     ) ;let
                    ) ;
                    (else (loop (+ p 1)))
              ) ;cond
            ) ;let
          ) ;let*
        ) ;and
      ) ;let
    ) ;define

    ;; HTML 标签/autolink 结束位置（跳过 label 扫描，Example 524/526/538）
    ;; 链接目标终止字符：`<` 或换行（§3.14）
    (define (link-stop-char? str p)
      (or (char=? (string-ref str p) #\<) (char=? (string-ref str p) #\newline))
    ) ;define

    (define (skip-html-or-autolink str pos)
      (let ((html-end (and (inline-html-start? str pos) (find-inline-html-end str pos)))
            (auto-result (and (autolink-start? str pos) (parse-autolink str pos)))
           ) ;
        (cond ((and html-end auto-result) (max html-end (cdr auto-result)))
              (html-end html-end)
              (auto-result (cdr auto-result))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (find-matching-bracket str start-pos)
      (let ((len (string-length str)))
        (let loop
          ((pos (+ start-pos 1)) (depth 1))
          (cond ((>= pos len) #f)
                ((char=? (string-ref str pos) #\\)
                 ;; 转义字符跳过（Example 194: [Foo*bar\]])
                 (if (< (+ pos 1) len) (loop (+ pos 2) depth) (loop (+ pos 1) depth))
                ) ;
                ((code-span-end str pos)
                 ;; code span 内的 ] 不算 label 结束（Example 342）
                 (loop (code-span-end str pos) depth)
                ) ;
                ((skip-html-or-autolink str pos)
                 ;; raw HTML/autolink 内的 ] [ 不算（Example 524/526/538）
                 (loop (skip-html-or-autolink str pos) depth)
                ) ;
                ((and (char=? (string-ref str pos) #\[)
                   (< (+ pos 1) len)
                   (not (char=? (string-ref str (+ pos 1)) #\]))
                 ) ;and
                 (loop (+ pos 1) (+ depth 1))
                ) ;
                ((char=? (string-ref str pos) #\])
                 (if (= depth 1) pos (loop (+ pos 1) (- depth 1)))
                ) ;
                (else (loop (+ pos 1) depth))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    ;; destination 解码反斜杠转义与字符引用，再 percent-encode（Example 32/33）
    (define (decode-link-dest str)
      (let* ((unescaped (decode-entity-references-in-string str))
             (decoded (decode-backslash-escapes-in-string unescaped))
            ) ;
        (percent-encode-uri decoded)
      ) ;let*
    ) ;define

    ;; title 解码统一在 (liii cmark common) 的 decode-title-escapes（见 1027 A8）

    (define (parse-link-destination dest)
      (let ((trimmed (string-trim dest)))
        (cond ((string=? trimmed "") (cons "" ""))
              ((string-find-char trimmed #\" 0) (parse-title trimmed #\" #\"))
              ((string-find-char trimmed #\' 0) (parse-title trimmed #\' #\'))
              ((string-find-char trimmed #\( 0) (parse-title trimmed #\( #\)))
              (else (cons (decode-link-dest (string-trim trimmed)) ""))
        ) ;cond
      ) ;let
    ) ;define
    (define (parse-title dest open-char close-char)
      (let ((quote-pos (string-find-char dest open-char 0)))
        (if quote-pos
          (let* ((url-part (decode-link-dest (string-trim (substring dest 0 quote-pos))))
                 (title-start (+ quote-pos 1))
                 (title-end (string-find-char dest close-char title-start))
                ) ;
            (if title-end
              (let ((title (decode-title-escapes (substring dest title-start title-end))))
                (cons url-part title)
              ) ;let
              (cons url-part "")
            ) ;if
          ) ;let*
          (cons (decode-link-dest (string-trim dest)) "")
        ) ;if
      ) ;let
    ) ;define
    (define (skip-inline-ws str start)
      (let ((len (string-length str)))
        (let loop
          ((p start))
          (if (and (< p len) (ascii-whitespace? (string-ref str p))) (loop (+ p 1)) p)
        ) ;let
      ) ;let
    ) ;define
    (define (find-bare-destination-end str start)
      (let ((len (string-length str)))
        (let loop
          ((p start) (depth 0))
          (cond ((>= p len) #f)
                ((char=? (string-ref str p) #\\) (if (< (+ p 1) len) (loop (+ p 2) depth) #f))
                ((or (char=? (string-ref str p) #\<) (char=? (string-ref str p) #\>)) #f)
                ((ascii-whitespace? (string-ref str p)) p)
                ((char=? (string-ref str p) #\() (loop (+ p 1) (+ depth 1)))
                ((char=? (string-ref str p) #\)) (if (= depth 0) p (loop (+ p 1) (- depth 1))))
                (else (loop (+ p 1) depth))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (parse-inline-link-destination str start)
      (let ((len (string-length str)))
        (cond ((>= start len) (cons "" start))
              ((char=? (string-ref str start) #\<)
               (let loop
                 ((p (+ start 1)))
                 (cond ((>= p len) #f)
                       ((char=? (string-ref str p) #\\) (if (< (+ p 1) len) (loop (+ p 2)) #f))
                       ((link-stop-char? str p) #f)
                       ((char=? (string-ref str p) #\>)
                        (cons (decode-link-dest (substring str (+ start 1) p)) (+ p 1))
                       ) ;
                       (else (loop (+ p 1)))
                 ) ;cond
               ) ;let
              ) ;
              ((char=? (string-ref str start) #\)) (cons "" start))
              (else
                (let ((end (find-bare-destination-end str start)))
                  (and end (cons (decode-link-dest (substring str start end)) end))
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define
    (define (parse-inline-link-title str start)
      (let ((len (string-length str)))
        (if (>= start len)
          (cons "" start)
          (let ((c (string-ref str start)))
            (if (or (char=? c #\") (char=? c #\') (char=? c #\())
              (let ((close-char (if (char=? c #\() #\) c)))
                (let loop
                  ((p (+ start 1)))
                  (cond ((>= p len) #f)
                        ((char=? (string-ref str p) #\\) (if (< (+ p 1) len) (loop (+ p 2)) #f))
                        ((char=? (string-ref str p) close-char)
                         (cons (decode-title-escapes (substring str (+ start 1) p)) (+ p 1))
                        ) ;
                        (else (loop (+ p 1)))
                  ) ;cond
                ) ;let
              ) ;let
              (cons "" start)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (parse-inline-link-rest str start)
      (let* ((len (string-length str))
             (ws1-pos (skip-inline-ws str start))
             (dest-result (parse-inline-link-destination str ws1-pos))
            ) ;
        (and dest-result
          (let* ((dest (car dest-result))
                 (ws2-pos (skip-inline-ws str (cdr dest-result)))
                 (title-result (parse-inline-link-title str ws2-pos))
                ) ;
            (and title-result
              (let* ((title (car title-result)) (ws3-pos (skip-inline-ws str (cdr title-result))))
                (and (< ws3-pos len)
                  (char=? (string-ref str ws3-pos) #\))
                  (cons (cons dest title) (+ ws3-pos 1))
                ) ;and
              ) ;let*
            ) ;and
          ) ;let*
        ) ;and
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library
