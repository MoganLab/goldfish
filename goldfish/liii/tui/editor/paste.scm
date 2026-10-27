;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

;; 粘贴内容折叠：大段粘贴在编辑器中只保留 [paste #N ...] 标记，
;; 真实内容存在注册表里，提交时再展开（对齐上游 pi 的 Editor 行为）。
(define-library (liii tui editor paste)
  (export %bracketed-paste? %bracketed-paste-content %paste-normalize
    %paste-large? make-paste-registry paste-registry? paste-registry-add!
    paste-registry-clear! paste-registry-count paste-registry-ref
    paste-registry-remove! paste-expand paste-marker-at-end
    paste-marker-at-start paste-renumber-lines
  ) ;export
  (import (scheme base) (liii base) (liii list) (liii string) (liii unicode))
  (begin

    (define %paste-start-marker "\x1b;[200~")
    (define %paste-end-marker "\x1b;[201~")
    (define %paste-prefix "[paste #")
    (define %paste-max-lines 10)
    (define %paste-max-chars 1000)

    ;; 终端把整段粘贴包装成 ESC[200~...ESC[201~ 交给编辑器
    (define (%bracketed-paste? text)
      (and (string? text)
        (let ((len (utf8-string-length text)))
          (and (>= len 12)
            (string=? (utf8-substring text 0 6) %paste-start-marker)
            (string=? (utf8-substring text (- len 6) len) %paste-end-marker)
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    (define (%bracketed-paste-content text)
      (if (%bracketed-paste? text)
        (utf8-substring text 6 (- (utf8-string-length text) 6))
        ""
      ) ;if
    ) ;define

    (define (%char-digit? ch)
      (and (char>=? ch #\0) (char<=? ch #\9))
    ) ;define

    ;; 按 Unicode 码点取字符；编辑器光标列也是码点索引。
    ;; string-ref 基于字节索引，不能直接用于多字节字符。
    (define (%char-at text i)
      (let ((chunk (utf8-substring text i (+ i 1))))
        (and (> (utf8-string-length chunk) 0)
          (integer->char (utf8->codepoint (string->utf8 chunk)))
        ) ;and
      ) ;let
    ) ;define

    ;; 按码点拆成单字符块；后续按块比较，避免反复解码码点
    (define (%string-chunks text)
      (let loop
        ((i 0) (acc '()))
        (if (>= i (utf8-string-length text))
          (reverse acc)
          (loop (+ i 1) (cons (utf8-substring text i (+ i 1)) acc))
        ) ;if
      ) ;let
    ) ;define

    (define (%count-newlines text)
      (let* ((bv (string->utf8 text)) (len (bytevector-length bv)))
        (let loop
          ((i 0) (n 0))
          (if (>= i len) n (loop (+ i 1) (if (= (bytevector-u8-ref bv i) 10) (+ n 1) n)))
        ) ;let
      ) ;let*
    ) ;define

    ;; 归一化：CRLF/CR -> LF，Tab -> 4 空格，过滤除换行外的控制字符
    (define (%paste-normalize text)
      (let loop
        ((chunks (%string-chunks text)) (acc '()))
        (cond ((null? chunks) (string-join (reverse acc) ""))
              ((string=? (car chunks) "\r")
               (let ((rest (cdr chunks)))
                 (if (and (pair? rest) (string=? (car rest) "\n"))
                   (loop (cdr rest) (cons "\n" acc))
                   (loop rest (cons "\n" acc))
                 ) ;if
               ) ;let
              ) ;
              ((string=? (car chunks) "\t") (loop (cdr chunks) (cons "    " acc)))
              ((and (= (string-length (car chunks)) 1)
                 (char<? (string-ref (car chunks) 0) #\space)
                 (not (char=? (string-ref (car chunks) 0) #\newline))
               ) ;and
               (loop (cdr chunks) acc)
              ) ;
              (else (loop (cdr chunks) (cons (car chunks) acc)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%paste-large? text)
      (let ((lines (+ 1 (%count-newlines text))) (chars (utf8-string-length text)))
        (or (> lines %paste-max-lines) (> chars %paste-max-chars))
      ) ;let
    ) ;define

    (define (%paste-marker id text)
      (let ((lines (+ 1 (%count-newlines text))) (chars (utf8-string-length text)))
        (if (> lines %paste-max-lines)
          (string-append "[paste #"
            (number->string id)
            " +"
            (number->string lines)
            " lines]"
          ) ;string-append
          (string-append "[paste #"
            (number->string id)
            " "
            (number->string chars)
            " chars]"
          ) ;string-append
        ) ;if
      ) ;let
    ) ;define

    ;; ---------------------------------------------------------------
    ;; 标记解析："[paste #12 1965 chars]" / "[paste #3 +12 lines]"
    ;; ---------------------------------------------------------------

    (define (%literal-at? text i lit)
      (let ((n (string-length lit)))
        (and (<= (+ i n) (utf8-string-length text))
          (string=? (utf8-substring text i (+ i n)) lit)
        ) ;and
      ) ;let
    ) ;define

    (define (%digits-at text i)
      (let ((len (utf8-string-length text)))
        (let loop
          ((j i))
          (if (and (< j len) (%char-digit? (%char-at text j)))
            (loop (+ j 1))
            (if (= j i) #f (cons (string->number (utf8-substring text i j)) j))
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; i 指向 id 之后的空格，返回 (close-index suffix) 或 #f
    (define (%parse-suffix text i)
      (let ((len (utf8-string-length text)))
        (if
          (and (< (+ i 1) len) (char=? (%char-at text (+ i 1)) #\+))
          (let ((num (%digits-at text (+ i 2))))
            (and num
              (%literal-at? text (cdr num) " lines]")
              (let ((close (+ (cdr num) 6)))
                (list close (utf8-substring text i close))
              ) ;let
            ) ;and
          ) ;let
          (let ((num (%digits-at text (+ i 1))))
            (and num
              (%literal-at? text (cdr num) " chars]")
              (let ((close (+ (cdr num) 6)))
                (list close (utf8-substring text i close))
              ) ;let
            ) ;and
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 返回 (id marker-end suffix) 或 #f；end 指向 ']' 之后
    (define (%marker-at text start)
      (and (%literal-at? text start %paste-prefix)
        (let ((num (%digits-at text (+ start (string-length %paste-prefix)))))
          (and num
            (let ((id (car num)) (i (cdr num)) (len (utf8-string-length text)))
              (cond
               ((and (< i len) (char=? (%char-at text i) #\])) (list id (+ i 1) ""))
               ((and (< i len) (char=? (%char-at text i) #\space))
                (let ((suffix (%parse-suffix text i)))
                  (and suffix (list id (+ (car suffix) 1) (cadr suffix)))
                ) ;let
               ) ;
               (else #f)
              ) ;cond
            ) ;let
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    ;; ---------------------------------------------------------------
    ;; 粘贴注册表
    ;; ---------------------------------------------------------------

    (define-record-type <paste-registry>
      (%make-paste-registry entries counter)
      paste-registry?
      (entries %paste-entries %paste-entries-set!)
      (counter %paste-counter %paste-counter-set!)
    ) ;define-record-type

    (define (make-paste-registry)
      (%make-paste-registry '() 0)
    ) ;define

    (define (paste-registry-count reg)
      (%paste-counter reg)
    ) ;define

    (define (paste-registry-ref reg id)
      (let ((entry (assoc id (%paste-entries reg))))
        (and entry (cdr entry))
      ) ;let
    ) ;define

    (define (paste-registry-add! reg text)
      (let ((id (+ (%paste-counter reg) 1)))
        (%paste-counter-set! reg id)
        (%paste-entries-set! reg (cons (cons id text) (%paste-entries reg)))
        (%paste-marker id text)
      ) ;let
    ) ;define

    (define (paste-registry-clear! reg)
      (%paste-entries-set! reg '())
      (%paste-counter-set! reg 0)
      #t
    ) ;define

    ;; 删除指定 id，并把更大的 id 整体减一（保持编号连续）
    (define (paste-registry-remove! reg id)
      (when (paste-registry-ref reg id)
        (%paste-entries-set! reg
          (map
            (lambda (entry)
              (if (> (car entry) id) (cons (- (car entry) 1) (cdr entry)) entry)
            ) ;lambda
            (filter
              (lambda (entry) (not (= (car entry) id)))
              (%paste-entries reg)
            ) ;filter
          ) ;map
        ) ;%paste-entries-set!
        (%paste-counter-set! reg (max 0 (- (%paste-counter reg) 1)))
        #t
      ) ;when
    ) ;define

    ;; 把文本中的标记展开为真实内容（只展开注册表里仍存在的 id）
    (define (paste-expand reg text)
      (let ((len (utf8-string-length text)))
        (let loop
          ((i 0) (acc '()))
          (if (>= i len)
            (string-join (reverse acc) "")
            (let ((marker (and (char=? (%char-at text i) #\[) (%marker-at text i))))
              (if marker
                (let ((content (paste-registry-ref reg (car marker))))
                  (if content
                    (loop (cadr marker) (cons content acc))
                    (loop (+ i 1) (cons "[" acc))
                  ) ;if
                ) ;let
                (loop (+ i 1) (cons (utf8-substring text i (+ i 1)) acc))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 光标前（line[0..col]）是否恰好以一个有效标记结尾，返回 (id . start-col)
    (define (paste-marker-at-end reg line col)
      (let ((len (utf8-string-length line)))
        (let loop
          ((i 0))
          (if (>= i col)
            #f
            (let ((marker (and (char=? (%char-at line i) #\[) (%marker-at line i))))
              (if (and marker (= (cadr marker) col) (paste-registry-ref reg (car marker)))
                (cons (car marker) i)
                (loop (+ i 1))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 光标处是否以一个有效标记开头，返回 (id . end-col)
    (define (paste-marker-at-start reg line col)
      (let ((len (utf8-string-length line)))
        (if (and (< col len) (char=? (%char-at line col) #\[))
          (let ((marker (%marker-at line col)))
            (and marker
              (paste-registry-ref reg (car marker))
              (cons (car marker) (cadr marker))
            ) ;and
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

    ;; 删除 id 后，文本里所有更大的标记编号减一（含用户手打的同形文本，
    ;; 与上游 pi 的 renumber 行为一致）
    (define (%renumber-line line target)
      (let ((len (utf8-string-length line)))
        (let loop
          ((i 0) (acc '()))
          (if (>= i len)
            (string-join (reverse acc) "")
            (let ((marker (and (char=? (%char-at line i) #\[) (%marker-at line i))))
              (if (and marker (> (car marker) target))
                (loop (cadr marker)
                  (cons
                    (string-append "[paste #"
                      (number->string (- (car marker) 1))
                      (list-ref marker 2)
                      "]"
                    ) ;string-append
                    acc
                  ) ;cons
                ) ;loop
                (loop (+ i 1) (cons (utf8-substring line i (+ i 1)) acc))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (paste-renumber-lines lines target)
      (map (lambda (line) (%renumber-line line target)) lines)
    ) ;define

  ) ;begin
) ;define-library
