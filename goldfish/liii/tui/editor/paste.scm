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

;; 粘贴内容折叠：超过阈值的粘贴在编辑器中只保留 [paste #N ...] 标记，
;; 原文保存在注册表里，提交时再由 paste-expand 展开。
;; 阈值：>10 行 或 >1000 字符；字符数优先于行数展示。
(define-library (liii tui editor paste)
  (export %bracketed-paste? %bracketed-paste-content %paste-normalize
    %paste-large? make-paste-registry paste-registry? paste-registry-add!
    paste-registry-clear! paste-registry-count paste-registry-remove!
    paste-expand paste-marker-at-end paste-marker-at-start
  ) ;export
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii unicode)
  ) ;import
  (begin

    (define %paste-start-marker "\x1b;[200~")
    (define %paste-end-marker "\x1b;[201~")
    (define %paste-max-lines 10)
    (define %paste-max-chars 1000)

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
      (let loop
        ((chunks (%string-chunks text)) (n 0))
        (cond ((null? chunks) n)
              ((string=? (car chunks) "\n") (loop (cdr chunks) (+ n 1)))
              (else (loop (cdr chunks) n))
        ) ;cond
      ) ;let
    ) ;define

    (define (%paste-lines text)
      (+ 1 (%count-newlines text))
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
      (or (> (%paste-lines text) %paste-max-lines)
        (> (utf8-string-length text) %paste-max-chars)
      ) ;or
    ) ;define

    ;; >1000 字符时展示字符数，否则展示总行数
    (define (%paste-marker id text)
      (let ((chars (utf8-string-length text)) (lines (%paste-lines text)))
        (if (> chars %paste-max-chars)
          (string-append "[paste #"
            (number->string id)
            " "
            (number->string chars)
            " chars]"
          ) ;string-append
          (string-append "[paste #"
            (number->string id)
            " +"
            (number->string lines)
            " lines]"
          ) ;string-append
        ) ;if
      ) ;let
    ) ;define

    ;; ---------------------------------------------------------------
    ;; 注册表：entry = (marker . content)
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

    (define (paste-registry-add! reg text)
      (let* ((id (+ (%paste-counter reg) 1))
             (marker (%paste-marker id text))
            ) ;
        (%paste-counter-set! reg id)
        (%paste-entries-set! reg (cons (cons marker text) (%paste-entries reg)))
        marker
      ) ;let*
    ) ;define

    (define (paste-registry-clear! reg)
      (%paste-entries-set! reg '())
      (%paste-counter-set! reg 0)
      #t
    ) ;define

    (define (paste-registry-remove! reg marker)
      (when (assoc marker (%paste-entries reg))
        (%paste-entries-set! reg
          (filter (lambda (entry) (not (string=? (car entry) marker)))
            (%paste-entries reg)
          ) ;filter
        ) ;%paste-entries-set!
        #t
      ) ;when
    ) ;define

    ;; 把文本中的标记展开为真实内容
    (define (paste-expand reg text)
      (let loop
        ((entries (%paste-entries reg)) (acc text))
        (if (null? entries)
          acc
          (loop (cdr entries) (string-replace acc (caar entries) (cdar entries)))
        ) ;if
      ) ;let
    ) ;define

    ;; 光标前是否恰好以一个有效标记结尾，返回 (marker . start-col) 或 #f
    (define (paste-marker-at-end reg line col)
      (let loop
        ((entries (%paste-entries reg)))
        (if (null? entries)
          #f
          (let* ((marker (caar entries)) (len (utf8-string-length marker)))
            (if (and (>= col len)
                  (string=? (utf8-substring line (- col len) col) marker)
                ) ;and
              (cons marker (- col len))
              (loop (cdr entries))
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 光标处是否以一个有效标记开头，返回 (marker . end-col) 或 #f
    (define (paste-marker-at-start reg line col)
      (let ((line-len (utf8-string-length line)))
        (let loop
          ((entries (%paste-entries reg)))
          (if (null? entries)
            #f
            (let* ((marker (caar entries)) (len (utf8-string-length marker)))
              (if (and (<= (+ col len) line-len)
                    (string=? (utf8-substring line col (+ col len)) marker)
                  ) ;and
                (cons marker (+ col len))
                (loop (cdr entries))
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
