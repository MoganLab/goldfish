
(define-library (liii cmark inline entity)
  (import (scheme base))
  (import (scheme char))
  (import (only (liii string) string-skip))
  (import (liii unicode))
  (import (liii cmark node))
  (import (liii cmark text))
  (import (liii cmark html-entities))
  (export entity-start? parse-entity-reference decode-entity-references-in-string)
  (begin
    ;; goldfish 字符串为字节导向，integer->char 仅接受 0x00..0xFF；
    ;; Unicode 码点须用 (liii unicode) 的 utf8-string 还原（Example 25-27/206）
    (define (codepoint->char-string cp)
      (utf8-string (integer->char cp))
    ) ;define

    (define (codepoints->string cps)
      (apply utf8-string (map integer->char cps))
    ) ;define

    ;; 完整 WHATWG 命名实体表（带分号形式），来自 (liii cmark html-entities)
    ;; 表中名字为 symbol，解析出的 name 为 string，故用 string->symbol 查表
    (define (named-entity-value name)
      (let ((cell (assoc (string->symbol name) *html-entities*)))
        (if cell (codepoints->string (cdr cell)) #f)
      ) ;let
    ) ;define

    (define (hex-digit? ch)
      (or (char-numeric? ch)
        (and (char>=? ch #\a) (char<=? ch #\f))
        (and (char>=? ch #\A) (char<=? ch #\F))
      ) ;or
    ) ;define

    (define (entity-start? str pos)
      (and (< pos (string-length str)) (char=? (string-ref str pos) #\&))
    ) ;define

    (define (entity-name-char? ch)
      (or (char-alphabetic? ch) (char-numeric? ch))
    ) ;define

    ;; Unicode 合法标量值: 排除 surrogate(0xD800..0xDFFF) 与 >0x10FFFF
    ;; 越界 / surrogate 值返回 #f，由调用方按原文保留（Example 28 的 &#87654321;）
    (define (unicode-scalar->string value)
      (cond ((= value 0) (codepoint->char-string 65533))
            ((and (<= value 1114111) (not (and (>= value 55296) (<= value 57343))))
             (codepoint->char-string value)
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (parse-numeric-entity-reference str start digits-start base)
      (let* ((pred (if (= base 16) hex-digit? char-numeric?))
             (pos (string-skip str pred digits-start))
            ) ;
        (if (and pos (char=? (string-ref str pos) #\;) (> pos digits-start))
          (let* ((value (string->number (substring str digits-start pos) base))
                 (decoded (and value (unicode-scalar->string value)))
                ) ;
            (if value
              (cons (or decoded (substring str start (+ pos 1))) (+ pos 1))
              (cons "&" (+ start 1))
            ) ;if
          ) ;let*
          (cons "&" (+ start 1))
        ) ;if
      ) ;let*
    ) ;define

    (define (parse-named-entity-reference str start)
      (let ((pos (string-skip str entity-name-char? (+ start 1))))
        (if (and pos (char=? (string-ref str pos) #\;) (> pos (+ start 1)))
          (let ((value (named-entity-value (substring str (+ start 1) pos))))
            (if value (cons value (+ pos 1)) (cons "&" (+ start 1)))
          ) ;let
          (cons "&" (+ start 1))
        ) ;if
      ) ;let
    ) ;define

    (define (parse-entity-reference-text str start)
      (let ((len (string-length str)))
        (if (not (entity-start? str start))
          (cons "" start)
          (cond
           ((and (< (+ start 2) len)
              (char=? (string-ref str (+ start 1)) #\#)
              (or (char=? (string-ref str (+ start 2)) #\x)
                (char=? (string-ref str (+ start 2)) #\X)
              ) ;or
            ) ;and
            (parse-numeric-entity-reference str start (+ start 3) 16)
           ) ;
           ((and (< (+ start 1) len) (char=? (string-ref str (+ start 1)) #\#))
            (parse-numeric-entity-reference str start (+ start 2) 10)
           ) ;
           (else (parse-named-entity-reference str start))
          ) ;cond
        ) ;if
      ) ;let
    ) ;define

    (define (parse-entity-reference str start)
      (let ((result (parse-entity-reference-text str start)))
        (cons (make-cmark-text (car result)) (cdr result))
      ) ;let
    ) ;define

    (define (decode-entity-references-in-string str)
      (let ((len (string-length str)))
        (let loop
          ((pos 0) (parts '()))
          (if (>= pos len)
            (string-join (reverse parts) "")
            (if (entity-start? str pos)
              (let ((result (parse-entity-reference-text str pos)))
                (loop (cdr result) (cons (car result) parts))
              ) ;let
              (loop (+ pos 1) (cons (string (string-ref str pos)) parts))
            ) ;if
          ) ;if
        ) ;let
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
