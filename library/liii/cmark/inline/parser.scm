(define-library (liii cmark inline parser)
  (import (scheme base) (liii base))
  (import (scheme char))
  (import (srfi srfi-1))
  (import (liii cmark text))
  (import (liii cmark inline char))
  (import (liii cmark inline emphasis))
  (import (liii cmark inline util))
  (import (liii cmark inline autolink))
  (import (liii cmark inline escape))
  (import (liii cmark inline emoji))
  (import (liii cmark inline html))
  (import (liii cmark block reference))
  (import (liii cmark inline math))
  (import (liii cmark inline emph-delimiters))
  (import (liii cmark inline emph-parser))
  (import (liii cmark inline link-parser))
  (import (liii cmark node))
  (export make-cmark-inline-parser cmark-inline-parser?
    cmark-inline-parser-content cmark-inline-parser-set-content!
    cmark-inline-parser-pos cmark-inline-parser-set-pos!
    cmark-inline-parser-node cmark-inline-parser-set-node!
    cmark-inline-parser-new parse-text inline-code-start? parse-inline-code
    emph-start? parse-emph strong-start? parse-strong strikeout-start?
    parse-strikeout link-start? parse-link image-start? parse-image
    autolink-start? parse-autolink bare-autolink-start? parse-bare-autolink
    emoji-start? parse-emoji inline-html-start? parse-inline-html linebreak?
    parse-linebreak parse-inlines
  ) ;export
  (begin
    ;; bare autolink 全局开关（goldfish 扩展，非 CommonMark）：默认开启
    (define *bare-autolink-enabled* #t)
    (define (bare-autolink-enabled?)
      *bare-autolink-enabled*
    ) ;define

    (define*-record-type <cmark-inline-parser>
      (make-cmark-inline-parser (content #f) (pos #f) (node #f))
      cmark-inline-parser?
      (content cmark-inline-parser-content cmark-inline-parser-set-content!)
      (pos cmark-inline-parser-pos cmark-inline-parser-set-pos!)
      (node cmark-inline-parser-node cmark-inline-parser-set-node!)
    ) ;define*-record-type
    (define (cmark-inline-parser-new)
      (make-cmark-inline-parser "" 0 #f)
    ) ;define
    (define (parse-text str)
      (make-cmark-text str)
    ) ;define

    ;; 解析文本片段（§3.11）
    (define (parse-inline-text str pos next-special)
      (parse-text (substring str pos next-special))
    ) ;define
    (define (inline-code-start? str pos)
      (and (< pos (string-length str)) (char=? (string-ref str pos) #\`))
    ) ;define
    ;; 换行转空格；两端各剥一个空格，但内容全为空格时不剥（Example 121/145/334）
    (define (normalize-code-span content)
      (let* ((spaced
               (string-map (lambda (c) (if (char=? c #\newline) #\space c)) content)
             ) ;spaced
             (len (string-length spaced))
            ) ;
        (if
          (and (>= len 2)
            (char=? (string-ref spaced 0) #\space)
            (char=? (string-ref spaced (- len 1)) #\space)
            (not (all-spaces? spaced))
          ) ;and
          (substring spaced 1 (- len 1))
          spaced
        ) ;if
      ) ;let*
    ) ;define

    ;; 全部字符均为空格（code span 内容全空格时保留原样，Example 334）

    (define (parse-inline-code str start)
      (let* ((len (string-length str))
             (marker-len (char-run-length str start #\`))
             (content-start (+ start marker-len))
             (end-pos (find-matching-backticks str content-start marker-len))
            ) ;
        (if end-pos
          (cons (make-cmark-code (normalize-code-span (substring str content-start end-pos)))
            (+ end-pos marker-len)
          ) ;cons
          ;; 未闭合 code span 只消费起始反引号，继续解析后续（避免吞掉整个剩余串）
          (cons (make-cmark-text (substring str start content-start)) content-start)
        ) ;if
      ) ;let*
    ) ;define
    ;; CommonMark flanking 规则（§6.2，Example 352-398）
    ;; 字符类别判定基于 UTF-8 解码后的码点（字节寻址下的多字节字符须解码，Example 394/406）
    ;; UTF-8 解码：返回 (cons codepoint char-len)；非法或越界 #f
    ;; 行首/行尾计为空白（CommonMark §6.2）

    (define (parse-em-or-strong str start . maybe-refmap)
      (let ((refmap (if (null? maybe-refmap) #f (car maybe-refmap))))
        (parse-em-or-strong-with str start refmap parse-inlines parse-link parse-image)
      ) ;let
    ) ;define
    (define (parse-emph str start . maybe-refmap)
      (let* ((refmap (if (null? maybe-refmap) #f (car maybe-refmap)))
             (result (parse-em-or-strong str start refmap))
            ) ;
        (cons (car (car result)) (cdr result))
      ) ;let*
    ) ;define
    (define (parse-strong str start . maybe-refmap)
      (let* ((refmap (if (null? maybe-refmap) #f (car maybe-refmap)))
             (result (parse-em-or-strong str start refmap))
            ) ;
        (cons (car (car result)) (cdr result))
      ) ;let*
    ) ;define
    (define (parse-strikeout str start . maybe-refmap)
      (let* ((len (string-length str))
             (refmap (if (null? maybe-refmap) #f (car maybe-refmap)))
             (end-pos (find-matching-strikeout str (+ start 2)))
            ) ;
        (if end-pos
          (let ((node (make-cmark-strikeout))
                (children (parse-inlines (substring str (+ start 2) end-pos) refmap))
               ) ;
            (for-each (lambda (child) (cmark-node-append-child! node child)) children)
            (cons node (+ end-pos 2))
          ) ;let
          (cons (make-cmark-text "~~") (+ start 2))
        ) ;if
      ) ;let*
    ) ;define
    (define (parse-link str start . maybe-refmap)
      (let ((refmap (if (null? maybe-refmap) #f (car maybe-refmap))))
        (parse-link-with str start refmap parse-inlines)
      ) ;let
    ) ;define
    (define (parse-image str start . maybe-refmap)
      (let ((refmap (if (null? maybe-refmap) #f (car maybe-refmap))))
        (parse-image-with str start refmap parse-inlines)
      ) ;let
    ) ;define
    (define (parse-inline-html str start)
      (let* ((len (string-length str)) (end-pos (find-inline-html-end str start)))
        (if end-pos
          (let* ((html-content (substring str start end-pos))
                 (html-node (make-cmark-html-inline html-content))
                ) ;
            (cons html-node end-pos)
          ) ;let*
          (cons (make-cmark-text "<") (+ start 1))
        ) ;if
      ) ;let*
    ) ;define
    (define (linebreak? str pos)
      (or (backslash-hard-linebreak? str pos)
        (and (< pos (string-length str)) (char=? (string-ref str pos) #\newline))
      ) ;or
    ) ;define
    (define (parse-linebreak str start)
      (cond ((backslash-hard-linebreak? str start)
             (cons (make-cmark-linebreak) (+ start 2))
            ) ;
            ((and (< start (string-length str)) (char=? (string-ref str start) #\newline))
             (if (hard-linebreak-by-spaces? str start)
               (cons (make-cmark-linebreak) (+ start 1))
               (cons (make-cmark-softbreak) (+ start 1))
             ) ;if
            ) ;
            (else (error 'parse-linebreak "linebreak expected at position" start))
      ) ;cond
    ) ;define
    (define (parse-inlines-math str pos len nodes)
      (let ((result (parse-math-inline str pos)))
        (if
          (and (< (+ pos 1) len)
            (char=? (string-ref str (+ pos 1)) #\$)
            (eq? (cmark-node-type (car result)) 'math_inline)
          ) ;and
          (let ((next-pos (cdr result))
                (trimmed-nodes (trim-trailing-spaces-from-last-text-node nodes))
               ) ;
            (if (and (< next-pos len) (char-whitespace? (string-ref str next-pos)))
              (cons (+ next-pos 1) (append-inline-node trimmed-nodes (car result)))
              (cons next-pos (append-inline-node trimmed-nodes (car result)))
            ) ;if
          ) ;let
          (cons (cdr result) (append-inline-node nodes (car result)))
        ) ;if
      ) ;let
    ) ;define

    (define (parse-inlines-plain str pos len nodes)
      (let ((next-special (find-next-special str pos)))
        (cond
         ((not next-special)
          (cons len (append-inline-node nodes (parse-text (substring str pos len))))
         ) ;
         ((= next-special pos)
          (cons (+ pos 1)
            (append-inline-node nodes (make-cmark-text (substring str pos (+ pos 1))))
          ) ;cons
         ) ;
         (else (cons next-special
                 (append-inline-node nodes (parse-inline-text str pos next-special))
               ) ;cons
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (parse-inlines str . maybe-refmap)
      (let ((len (string-length str)))
        (let ((refmap (if (null? maybe-refmap) #f (car maybe-refmap))))
          (let loop
            ((pos 0) (nodes '()))
            (cond ((>= pos len) (reverse nodes))
                  ((inline-code-start? str pos)
                   (let ((result (parse-inline-code str pos)))
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let
                  ) ;
                  ((or (strong-start? str pos) (emph-start? str pos))
                   (let ((result (parse-em-or-strong str pos refmap)))
                     (loop (cdr result)
                       (let append-nodes
                         ((remaining (car result)) (acc nodes))
                         (if (null? remaining)
                           acc
                           (append-nodes (cdr remaining) (append-inline-node acc (car remaining)))
                         ) ;if
                       ) ;let
                     ) ;loop
                   ) ;let
                  ) ;
                  ((strikeout-start? str pos)
                   (let ((result (parse-strikeout str pos refmap)))
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let
                  ) ;
                  ((image-start? str pos)
                   (let ((result (parse-image str pos refmap)))
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let
                  ) ;
                  ((link-start? str pos)
                   (let ((result (parse-link str pos refmap)))
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let
                  ) ;
                  ((emoji-start? str pos)
                   (let ((result (parse-emoji str pos)))
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let
                  ) ;
                  ((or (autolink-start? str pos) (inline-html-start? str pos))
                   (let* ((a-result (and (autolink-start? str pos) (parse-autolink str pos)))
                          (result (or a-result (parse-inline-html str pos)))
                         ) ;
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let*
                  ) ;
                  ((and (bare-autolink-enabled?) (bare-autolink-start? str pos))
                   (let ((result (parse-bare-autolink str pos)))
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let
                  ) ;
                  ((and (< pos len) (char=? (string-ref str pos) #\<))
                   (loop (+ pos 1) (append-inline-node nodes (make-cmark-text "<")))
                  ) ;
                  ((and (< pos len) (char=? (string-ref str pos) #\~))
                   (loop (+ pos 1) (append-inline-node nodes (make-cmark-text "~")))
                  ) ;
                  ((and (< pos len) (char=? (string-ref str pos) #\:))
                   (loop (+ pos 1) (append-inline-node nodes (make-cmark-text ":")))
                  ) ;
                  ((backslash-hard-linebreak? str pos)
                   (loop (+ pos 2)
                     (append-inline-node (trim-trailing-spaces-from-last-text-node nodes)
                       (make-cmark-linebreak)
                     ) ;append-inline-node
                   ) ;loop
                  ) ;
                  ;; \( 是 math 定界（数学扩展），须在 backslash-char? 转义分支之前；
                  ;; \\( 由转义分支先消费 \\，主循环不会停在被转义的 \( 上
                  ((math-inline-start? str pos)
                   (let ((res (parse-inlines-math str pos len nodes)))
                     (loop (car res) (cdr res))
                   ) ;let
                  ) ;
                  ((backslash-char? str pos)
                   (let ((result (parse-backslash-escape str pos)))
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let
                  ) ;
                  ((entity-start? str pos)
                   (let* ((result (parse-entity-reference str pos))
                          (decoded (node-data-ref (car result) 'content ""))
                          (updated-nodes (append-entity-text-nodes nodes decoded))
                         ) ;
                     (loop (cdr result) updated-nodes)
                   ) ;let*
                  ) ;
                  ((linebreak? str pos)
                   (let* ((result (parse-linebreak str pos))
                          (node (car result))
                          (updated-nodes
                            (if (or (eq? (cmark-node-type node) 'linebreak)
                                  (eq? (cmark-node-type node) 'softbreak)
                                ) ;or
                              (trim-trailing-spaces-from-last-text-node nodes)
                              nodes
                            ) ;if
                          ) ;updated-nodes
                         ) ;
                     (loop (cdr result) (append-inline-node updated-nodes node))
                   ) ;let*
                  ) ;
                  (else
                    (let ((res (parse-inlines-plain str pos len nodes)))
                      (loop (car res) (cdr res))
                    ) ;let
                  ) ;else
            ) ;cond
          ) ;let
        ) ;let
      ) ;let
    ) ;define
    (define (append-entity-text-nodes nodes decoded)
      (let ((len (string-length decoded)))
        (let loop
          ((pos 0) (start 0) (acc nodes))
          (cond
           ((>= pos len)
            (if (= pos start)
              acc
              (append-inline-node acc (make-cmark-text (substring decoded start pos)))
            ) ;if
           ) ;
           ((char=? (string-ref decoded pos) #\newline)
            (let* ((text-node (make-cmark-text (substring decoded start pos)))
                   (acc2 (if (= pos start) acc (append-inline-node acc text-node)))
                  ) ;
              (loop (+ pos 1) (+ pos 1) (append-inline-node acc2 (make-cmark-softbreak)))
            ) ;let*
           ) ;
           (else (loop (+ pos 1) start acc))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
